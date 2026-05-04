#lang racket
; ייצוא כל הפונקציות שהוגדרו בקובץ זה
(provide (all-defined-out))

; משתנים לניהול המצב הנוכחי של הפרסר
(define commands-list '()) ; רשימת כל הפקודות הנקיות בקובץ
(define current-command "") ; הפקודה הנוכחית שמנתחים

; פונקציה שמנקה רווחים בשורה של קובץ
(define (clean-line line)
  (let ([trimmed (string-trim line)]) ; מנקה רווחים מהצדדים קודם כל
    (cond
      [(string=? trimmed "") ""] ; מטפל בשורה ריקה
      [(string-prefix? trimmed "//") ""] ; אם השורה מתחילה ב-//, היא כולה הערה - מחזיר מחרוזת ריקה
      [else 
       ; אם יש פקודה ואחריה הערה (כמו push constant 7 // comment)
       (let ([parts (string-split trimmed "//")])
         (string-trim (first parts)))])))

; --- Constructor ---
; פותח את הקובץ, מנקה הערות ושורות ריקות ושומר את הפקודות ברשימה
(define (parser-init file-path)
  ; אנחנו משתמשים ב-port->lines בתוך הבלוק כדי לקרוא הכל מיד
  (let ([lines (with-input-from-file file-path 
                 (lambda () (port->lines)))]) 
    (set! commands-list 
          (filter (lambda (l) (not (string=? l "")))
                  (map clean-line lines)))
    (set! current-command "")))

; --- hasMoreCommands ---
; מחזיר אמת אם נשארו עוד פקודות לעבד
(define (has-more-commands?)
  (not (empty? commands-list)))

; --- advance ---
; קורא את הפקודה הבאה מהקלט והופך אותה לפקודה הנוכחית
(define (advance!)
  (set! current-command (first commands-list))
  (set! commands-list (rest commands-list)))

; --- commandType ---
;  מחזיר את סוג הפקודה הנוכחית
(define (command-type)
  (let ([cmd (first (string-split current-command))])
    (cond
      [(member cmd '("add" "sub" "neg" "eq" "gt" "lt" "and" "or" "not", "#sub")) 'C_ARITHMETIC]
      [(string=? cmd "push") 'C_PUSH]
      [(string=? cmd "pop")  'C_POP]
      ; פקודות לפרויקט הבא:
      [(string=? cmd "label") 'C_LABEL]
      [(string=? cmd "goto")  'C_GOTO]
      [(string=? cmd "if-goto") 'C_IF]
      [(string=? cmd "function") 'C_FUNCTION]
      [(string=? cmd "return") 'C_RETURN]
      [(string=? cmd "call") 'C_CALL]
      [else 'UNKNOWN])))

; --- arg1 ---
; מחזיר את הארגומנט הראשון. עבור פקודה אריתמטית מחזיר את הפקודה עצמה
(define (arg1)
  (let ([parts (string-split current-command)]
        [type (command-type)])
    (if (eq? type 'C_ARITHMETIC) (first parts)
        (second parts))))


; --- arg2 ---
; מחזיר את הארגומנט השני (רק עבור push, pop, function, call)
(define (arg2)
  (string->number (third (string-split current-command))))     