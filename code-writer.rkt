#lang racket

; מייצא (חושף) את הפונקציות האלה כדי שקבצים אחרים יוכלו להשתמש בהן.
(provide set-file-name! write-arithmetic write-push-pop 
         write-label write-goto write-if write-init 
         write-function write-call write-return)

; מגדיר משתנה גלובלי לשמירת שם הקובץ הנוכחי
(define current-file-name "")

; מגדיר משתנה גלובלי לשמירת שם הקובץ הנוכחי
(define label-counter 0)
; מגדיר פונקציה שמקבלת שם קובץ

(define (set-file-name! name)
  (set! current-file-name name)) ; מעדכן את המשתנה הגלובלי עם השם החדש

; פונקציה שיוצרת תווית חדשה עם קידומת
(define (get-new-label prefix)
  (set! label-counter (+ label-counter 1)) ; הגדלת המונה ב1
  (format "~a.~a" prefix label-counter)) ; TRUE.1, END.2 יוצר מחרוזת כמו

 ; ---פעולות אריתמטיות---
 ; ומפעילה את הפקודה המתאימה (add,sub,neg,lt,gt,eq,and,not,or) פונקציה שמקבלת אחת מהפקודות האריתמטיות הבאות
(define (write-arithmetic command)
  (string-append 
   "// " command "\n" ; שרשור שם הפקודה כהערה
   (cond
     [(member command '("add" "sub" "and" "or")) (binary-op command)] ; בדיקה האם הפקודה היא בינארית
     [(member command '("neg" "not")) (unary-op command)] ; בדיקה האם הפקודה היא אונרית
     [(member command '("eq" "gt" "lt")) (compare-op command)] ; בדיקה האם הפקודה היא השוואה
     [else ""])))

; פונקציה ליצירת קוד האסמבלי לפקודות הבינריות
(define (binary-op op)
  (let ([asm-op (cond [(string=? op "add") "D+M"]
                      [(string=? op "sub") "M-D"]
                      [(string=? op "and") "D&M"]
                      [(string=? op "or")  "D|M"])])
    (string-append
     "@SP\n"
     "AM=M-1\n" ; הקטנת SP וגישה לאיבר העליון
     "D=M\n"    ; שמירת הערך ב-D
     "A=A-1\n"  ; מעבר לאיבר הבא (מתחתיו) במחסנית
     "M=" asm-op "\n"))) ; ביצוע הפעולה ועדכון (אין צורך להגדיל את SP חזרה!)

; פונקציה ליצירת קוד האסמבלי לפקודות האונריות
(define (unary-op op)
  (let ([asm-op (if (string=? op "neg") "-M" "!M")]) ;אם הפקודה היא נג שים מינוס אחרת שים סימן קריאה
    (string-append
     "@SP\n"
     "A=M-1\n" ; גישה לאיבר בראש המחסנית (בלי לשנות את SP)
     "M=" asm-op "\n")))

; פונקציה ליצירת קוד האסמבלי לפקודות ההשוואה
(define (compare-op op)
  (let ([jmp (cond [(string=? op "eq") "JEQ"]
                   [(string=? op "gt") "JGT"]
                   [(string=? op "lt") "JLT"])]
        [true-label (get-new-label "TRUE")]
        [end-label  (get-new-label "END")])
    (string-append
     "@SP\n"
     "AM=M-1\n" ; Pop first operand
     "D=M\n"
     "A=A-1\n"  ; Point to second operand
     "D=M-D\n"  ; Calculate difference
     "M=-1\n"   ; Default to True (-1)
     "@" true-label "\n"
     "D;" jmp "\n"
     "@SP\n"    ; If not true:
     "A=M-1\n"
     "M=0\n"    ; Set to False (0)
     "(" true-label ")\n")))

; --- פקודות Push / Pop ---
(define (write-push-pop type segment index)
  (string-append
   "// " type " " segment " " (number->string index) "\n"
   (if (string=? type "C_PUSH")
       (push-logic segment index)
       (pop-logic segment index))))

(define (push-logic segment index)
  (string-append
   (cond
     [(string=? segment "constant") 
      (format "@~a\nD=A\n" index)] ; push constant [cite: 1052]
     [(string=? segment "static")   
      (format "@~a.~a\nD=M\n" current-file-name index)] ; static variables 
     [(string=? segment "pointer")  
      (format "@~a\nD=M\n" (+ 3 index))] ; pointer 0/1 [cite: 1056]
     [(string=? segment "temp")     
      (format "@~a\nD=M\n" (+ 5 index))] ; temp 0-7 [cite: 1034]
     [else 
      (format "@~a\nD=A\n@~a\nA=M+D\nD=M\n" index (segment->base segment))]) ; local/arg/this/that [cite: 1039]
   "@SP\n"
   "A=M\n"
   "M=D\n"
   "@SP\n"
   "M=M+1\n"))

(define (pop-logic segment index)
  (cond
    [(string=? segment "pointer") 
     (format "@SP\nAM=M-1\nD=M\n@~a\nM=D\n" (+ 3 index))]
    [(string=? segment "temp")    
     (format "@SP\nAM=M-1\nD=M\n@~a\nM=D\n" (+ 5 index))]
    [(string=? segment "static")  
     (format "@SP\nAM=M-1\nD=M\n@~a.~a\nM=D\n" current-file-name index)]
    [else 
     ; Pop לסגמנטים עם בסיס משתנה (local, arg, etc.) דורש שמירת הכתובת ב-R13 [cite: 1089, 1099]
     (format "@~a\nD=A\n@~a\nD=M+D\n@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D\n" 
             index (segment->base segment))]))

(define (segment->base s)
  (cond [(string=? s "local") "LCL"]
        [(string=? s "argument") "ARG"]
        [(string=? s "this") "THIS"]
        [(string=? s "that") "THAT"])) 

; תרגיל 2 

; משתנה למעקב אחרי שם הפונקציה הנוכחית (עבור תוויות)
(define current-function-name "global")
(define call-counter 0) ; מונה ייחודי לקריאות לפונקציות
; --- פקודות בקרה (Branching) ---
(define (write-label label)
  (format "(~a$~a)\n" current-function-name label))
(define (write-goto label)
  (format "@~a$~a\n0;JMP\n" current-function-name label))
(define (write-if label)
  (string-append
   "@SP\n"
   "AM=M-1\n"
   "D=M\n"
   (format "@~a$~a\n" current-function-name label)
   "D;JNE\n")) ; קופץ אם הערך במחסנית אינו 0

   ; --- פונקציות וקריאות ---

(define (write-init)
  (string-append
   "// Bootstrap\n"
   "@256\nD=A\n@SP\nM=D\n" ; SP = 256
   (write-call "Sys.init" 0))) ; קריאה ל-Sys.init

(define (write-function function-name num-vars)
  (set! current-function-name function-name)
  (let ([header (format "(~a)\n" function-name)]
        [init-vars (apply string-append 
                          (make-list num-vars "@SP\nA=M\nM=0\n@SP\nM=M+1\n"))])
    (string-append header init-vars)))

(define (write-call function-name num-args)
  (set! call-counter (+ call-counter 1))
  (let ([return-label (format "RET_~a_~a" function-name call-counter)])
    (string-append
     ; push return-address
     (format "@~a\nD=A\n" return-label) (push-d-to-stack)
     ; push LCL, ARG, THIS, THAT
     "@LCL\nD=M\n" (push-d-to-stack)
     "@ARG\nD=M\n" (push-d-to-stack)
     "@THIS\nD=M\n" (push-d-to-stack)
     "@THAT\nD=M\n" (push-d-to-stack)
     ; ARG = SP - 5 - num-args
     "@SP\nD=M\n@5\nD=D-A\n@" (number->string num-args) "\nD=D-A\n@ARG\nM=D\n"
     ; LCL = SP
     "@SP\nD=M\n@LCL\nM=D\n"
     ; goto function-name
     "@" function-name "\n0;JMP\n"
     ; (return-address)
     "(" return-label ")\n")))

; פונקציית עזר פנימית ל-push
(define (push-d-to-stack)
  "@SP\nA=M\nM=D\n@SP\nM=M+1\n")

(define (write-return)
  (string-append
   "// --- Return --- \n"
   ; FRAME = LCL (משתמשים ב-R13 כמשתנה זמני ל-FRAME)
   "@LCL\nD=M\n@R13\nM=D\n"
   ; RET = *(FRAME - 5) (שומרים את כתובת החזרה ב-R14)
   "@5\nA=D-A\nD=M\n@R14\nM=D\n"
   ; *ARG = pop() (הצבת ערך החזרה במקום הארגומנט הראשון של הקורא)
   "@SP\nAM=M-1\nD=M\n@ARG\nA=M\nM=D\n"
   ; SP = ARG + 1 (החזרת המחסנית למצב שאחרי הקריאה)
   "@ARG\nD=M+1\n@SP\nM=D\n"
   ; שחזור פוינטרים מה-Frame הקודם (THAT, THIS, ARG, LCL)
   "@R13\nAM=M-1\nD=M\n@THAT\nM=D\n"
   "@R13\nAM=M-1\nD=M\n@THIS\nM=D\n"
   "@R13\nAM=M-1\nD=M\n@ARG\nM=D\n"
   "@R13\nAM=M-1\nD=M\n@LCL\nM=D\n"
   ; goto RET (קפיצה חזרה לכתובת השמורה)
   "@R14\nA=M\n0;JMP\n"))