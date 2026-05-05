#lang racket

; ייבוא המודולים האחרים
(require "parser.rkt")
(require "code-writer.rkt")

(define (run-translator dir-path)
  (let* ([files (filter (lambda (f) (regexp-match? #rx"\\.vm$" (path->string f)))
                        (directory-list dir-path))]
         #|[dir-name (last (string-split dir-path "/"))] |#
          ; הופך את הנתיב לאובייקט נתיב אמיתי ומחלץ את החלק האחרון שלו
         [path-obj (simple-form-path dir-path)]
         [dir-name (path->string (last (explode-path path-obj)))]
         ; קובץ הפלט יהיה כשם התיקייה עם סיומת .asm
         [output-path (build-path dir-path (string-append dir-name ".asm"))])
    
    (call-with-output-file output-path
      (lambda (out-port)
      ; בדיקה: אם יש יותר מקובץ אחד, או אם זה קובץ שמצפה לאתחול, כותבים Bootstrap
        (when (> (length files) 1)
          (display (write-init) out-port))
        ; לולאה על כל הקבצים שנמצאו
        (for ([file files])
        (printf "--- Reading file: ~a ---\n" (path->string file)) ; הדפסה איזה קובץ מטופל כרגע
          (let* ([file-path (build-path dir-path file)]
                 [file-name (path->string file)] 
                 [base-name (regexp-replace #rx"\\.vm$" file-name "")])
            
            ; 1. עדכון שם הקובץ ב-CodeWriter (עבור משתני static)
            (set-file-name! base-name)
            
            ; 2. אתחול ה-Parser עבור הקובץ הנוכחי
            (parser-init file-path)
            
            ; 3. לולאת התרגום המרכזית
            (translate-current-file out-port))))
      #:exists 'replace))) 

(define (translate-current-file out-port)
  (while (has-more-commands?)
    (advance!)
    (let ([type (command-type)])
      (cond
        ; טיפול בפקודות אריתמטיות
        [(eq? type 'C_ARITHMETIC)
         (display (write-arithmetic (arg1)) out-port)]
        
        ; טיפול בפקודות Push/Pop
        [(or (eq? type 'C_PUSH) (eq? type 'C_POP))
         (display (write-push-pop (symbol->string type) (arg1) (arg2)) out-port)]
        
        ; פקודות חדשות של שלב 2 
        [(eq? type 'C_LABEL)    (display (write-label (arg1)) out-port)]
        [(eq? type 'C_GOTO)     (display (write-goto (arg1)) out-port)]
        [(eq? type 'C_IF)       (display (write-if (arg1)) out-port)]
        [(eq? type 'C_FUNCTION) (display (write-function (arg1) (arg2)) out-port)]
        [(eq? type 'C_CALL)     (display (write-call (arg1) (arg2)) out-port)]
        [(eq? type 'C_RETURN)   (display (write-return) out-port)]))))


(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

; קריאת הארגומנטים שנשלחו מהטרמינל
(define args (current-command-line-arguments))

; בדיקה: אם נשלח בדיוק נתיב אחד - נריץ. אם לא - נדפיס הוראות
(if (= (vector-length args) 1)
    (run-translator (vector-ref args 0))
    (display "Usage: racket main.rkt <path-to-directory>\n"))