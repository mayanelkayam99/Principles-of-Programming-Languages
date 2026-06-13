#lang racket
(require "SymbolTable.rkt")
(require "vm-writer.rkt")
(require racket/string)
(require "tokenizer.rkt")

;; =================================================================
;; עזרים גלובליים
;; =================================================================

;; מונה גלובלי ליצירת תוויות ייחודיות (Labels) עבור if / while
(define label-counter 0)
(define (generate-label prefix)
  (set! label-counter (+ label-counter 1))
  (format "~a~a" prefix label-counter))

(define (get-token-value xml-token-string)
  (define match (regexp-match #px"<[^>]+>\\s*(.*?)\\s*</[^>]+>" xml-token-string))
  (if match (cadr match) ""))

(define (is-op? token-val)
  (member token-val '("+" "-" "*" "/" "&amp;" "|" "&lt;" "&gt;" "=" "<" ">" "&")))

(define (op->vm op)
  (cond
    [(string=? op "+") "add\n"]
    [(string=? op "-") "sub\n"]
    [(string=? op "*") "call Math.multiply 2\n"]
    [(string=? op "/") "call Math.divide 2\n"]
    [(or (string=? op "&amp;") (string=? op "&")) "and\n"]
    [(string=? op "|") "or\n"]
    [(or (string=? op "&lt;") (string=? op "<")) "lt\n"]
    [(or (string=? op "&gt;") (string=? op ">")) "gt\n"]
    [(string=? op "=") "eq\n"]
    [else ""]))

;; מתרגם את ה-Kind מהטבלה לשם המקטע ב-VM (שדה המחלקה יושב ב-this)
(define (kind->segment kind)
  (if (eq? kind 'field) "this" (symbol->string kind)))

;; =================================================================
;; הצהרות (מעדכנות ומחזירות את טבלת הסמלים)
;; =================================================================

(define (compile-class-var-dec tokens st)
  (define kind (string->symbol (get-token-value (car tokens))))
  (define type (get-token-value (cadr tokens)))
  (define (parse-names current-tokens current-st)
    (define var-name (get-token-value (car current-tokens)))
    (define next-st (symbol-table-add current-st var-name type kind))
    (define next-val (get-token-value (cadr current-tokens)))
    (if (string=? next-val ";")
        (cons next-st (cddr current-tokens))
        (parse-names (cddr current-tokens) next-st)))
  (parse-names (cddr tokens) st))

(define (compile-parameter-list tokens st)
  (define first-val (get-token-value (car tokens)))
  (if (string=? first-val ")")
      (cons st tokens)
      (let loop ([curr-tokens tokens] [curr-st st])
        (define type (get-token-value (car curr-tokens)))
        (define name (get-token-value (cadr curr-tokens)))
        (define next-st (symbol-table-add curr-st name type 'argument))
        (define next-val (get-token-value (caddr curr-tokens)))
        (if (string=? next-val ",")
            (loop (cdddr curr-tokens) next-st)
            ;; כאן התיקון: cddr במקום cdddr כדי להשאיר את ה-')' בזרם הטוקנים
            (cons next-st (cddr curr-tokens))))))
(define (compile-var-dec tokens st)
  (define type (get-token-value (cadr tokens)))
  (define (parse-names curr-tokens curr-st)
    (define name (get-token-value (car curr-tokens)))
    (define next-st (symbol-table-add curr-st name type 'local))
    (define next-val (get-token-value (cadr curr-tokens)))
    (if (string=? next-val ";")
        (cons next-st (cddr curr-tokens))
        (parse-names (cddr curr-tokens) next-st)))
  (parse-names (cddr tokens) st))

;; =================================================================
;; ביטויים - מחזירים (cons vm-code remaining-tokens)
;; =================================================================

(define (compile-expression-list tokens st class-name)
  (define (collect-exprs curr-tokens count vm-code)
    (define first-val (get-token-value (car curr-tokens)))
    (if (string=? first-val ")")
        (list vm-code count curr-tokens)
        (let* ((exp-res (compile-expression curr-tokens st class-name))
               (exp-vm (car exp-res))
               (rem (cdr exp-res)))
          (if (string=? (get-token-value (car rem)) ",")
              (collect-exprs (cdr rem) (+ count 1) (string-append vm-code exp-vm))
              (list (string-append vm-code exp-vm) (+ count 1) rem)))))
  (collect-exprs tokens 0 ""))

(define (compile-subroutine-call first-token next-token tokens st class-name)
  (define first-val (get-token-value first-token))
  (define next-val (get-token-value next-token))
  
  (if (string=? next-val ".")
      ;; קריאה מהסוג: Obj.method() או Class.function()
      (let* ((obj-name first-val)
             (func-name (get-token-value (caddr tokens)))
             (info (symbol-table-lookup st obj-name))
             (rem-tokens (cddddr tokens))) ;; אחרי ה-'('
        
        (if info
            ;; זוהי מתודה על אובייקט (מכניסים את האובייקט למחסנית)
            (let* ((type (var-info-type info))
                   (push-obj (string-append (write-push (kind->segment (var-info-kind info)) (var-info-index info)) "\n"))
                   (list-res (compile-expression-list rem-tokens st class-name))
                   (list-vm (car list-res))
                   (arg-count (+ (cadr list-res) 1))
                   (rem-after (caddr list-res))
                   (call-vm (string-append (write-call (format "~a.~a" type func-name) arg-count) "\n")))
              (cons (string-append push-obj list-vm call-vm) (cdr rem-after)))
            ;; זוהי פונקציה סטטית (Class.func)
            (let* ((list-res (compile-expression-list rem-tokens st class-name))
                   (list-vm (car list-res))
                   (arg-count (cadr list-res))
                   (rem-after (caddr list-res))
                   (call-vm (string-append (write-call (format "~a.~a" obj-name func-name) arg-count) "\n")))
              (cons (string-append list-vm call-vm) (cdr rem-after)))))
              
      ;; קריאה מהסוג: method() -> מופעל על this
      (let* ((func-name first-val)
             (rem-tokens (cddr tokens)) ;; אחרי ה-'('
             (push-this (string-append (write-push "pointer" 0) "\n"))
             (list-res (compile-expression-list rem-tokens st class-name))
             (list-vm (car list-res))
             (arg-count (+ (cadr list-res) 1))
             (rem-after (caddr list-res))
             (call-vm (string-append (write-call (format "~a.~a" class-name func-name) arg-count) "\n")))
        (cons (string-append push-this list-vm call-vm) (cdr rem-after)))))

(define (compile-term tokens st class-name)
  (define first-token (car tokens))
  (define first-val (get-token-value first-token))
  
  (cond
    ;; מספרים
    [(string-contains? first-token "<integerConstant>")
     (cons (string-append (write-push "constant" first-val) "\n") (cdr tokens))]
     
    ;; מחרוזות
    [(string-contains? first-token "<stringConstant>")
     (let* ((str-len (string-length first-val))
            (create-vm (string-append (write-push "constant" str-len) "\n" (write-call "String.new" 1) "\n"))
            (append-chars
             (string-join (map (lambda (char) 
                                 (string-append (write-push "constant" (char->integer char)) "\n"
                                                (write-call "String.appendChar" 2) "\n"))
                               (string->list first-val)) "")))
       (cons (string-append create-vm append-chars) (cdr tokens)))]
       
    ;; מילים שמורות (true, false, null, this)
    [(string-contains? first-token "<keyword>")
     (define vm (cond [(string=? first-val "true") "push constant 0\nnot\n"]
                      [(or (string=? first-val "false") (string=? first-val "null")) "push constant 0\n"]
                      [(string=? first-val "this") "push pointer 0\n"]
                      [else ""]))
     (cons vm (cdr tokens))]
     
    ;; אופרטור אונרי (~, -)
    [(or (string=? first-val "-") (string=? first-val "~"))
     (let* ((term-res (compile-term (cdr tokens) st class-name))
            (term-vm (car term-res))
            (op-vm (if (string=? first-val "-") "neg\n" "not\n")))
       (cons (string-append term-vm op-vm) (cdr term-res)))]
       
    ;; סוגריים ( expression )
    [(string=? first-val "(")
     (let* ((exp-res (compile-expression (cdr tokens) st class-name))
            (exp-vm (car exp-res)))
       (cons exp-vm (cdr (cdr exp-res))))] ;; מדלגים על ')'
       
    ;; מזהה (משתנה, מערך או פונקציה)
    [(string-contains? first-token "<identifier>")
     (define next-val (if (null? (cdr tokens)) "" (get-token-value (cadr tokens))))
     (cond
       ;; מערך a[i]
       [(string=? next-val "[")
        (let* ((info (symbol-table-lookup st first-val))
               (push-arr (string-append (write-push (kind->segment (var-info-kind info)) (var-info-index info)) "\n"))
               (exp-res (compile-expression (cddr tokens) st class-name))
               (exp-vm (car exp-res))
               (rem (cdr exp-res)) ;; הגענו ל-']'
               ;; חישוב הכתובת וקריאת הערך
               (arr-vm (string-append push-arr exp-vm "add\npop pointer 1\npush that 0\n")))
          (cons arr-vm (cdr rem)))]
          
       ;; קריאה לפונקציה
       [(or (string=? next-val "(") (string=? next-val "."))
        (compile-subroutine-call first-token (cadr tokens) tokens st class-name)]
        
       ;; משתנה פשוט
       [else
        (let ((info (symbol-table-lookup st first-val)))
          (cons (string-append (write-push (kind->segment (var-info-kind info)) (var-info-index info)) "\n")
                (cdr tokens)))])]
    [else (cons "" (cdr tokens))]))

(define (compile-expression tokens st class-name)
  (define term-res (compile-term tokens st class-name))
  (define term-vm (car term-res))
  (define rem (cdr term-res))
  
  (define (collect-ops curr-tokens acc-vm)
    (if (null? curr-tokens)
        (cons acc-vm curr-tokens)
        (let ((next-val (get-token-value (car curr-tokens))))
          (if (is-op? next-val)
              (let* ((next-term-res (compile-term (cdr curr-tokens) st class-name))
                     (next-term-vm (car next-term-res))
                     (rem-tokens (cdr next-term-res))
                     (op-vm (op->vm next-val)))
                ;; משרשרים בסדר של RPN: קודם האיבר השני, ואז האופרטור
                (collect-ops rem-tokens (string-append acc-vm next-term-vm op-vm)))
              (cons acc-vm curr-tokens)))))
              
  (collect-ops rem term-vm))

;; =================================================================
;; פקודות (Statements)
;; =================================================================

(define (compile-let tokens st class-name)
  (define var-name (get-token-value (cadr tokens)))
  (define next-val (get-token-value (caddr tokens)))
  (define is-array (string=? next-val "["))
  
  (if is-array
      ;; השמה למערך: let a[i] = expr
      (let* ((info (symbol-table-lookup st var-name))
             (push-arr (string-append (write-push (kind->segment (var-info-kind info)) (var-info-index info)) "\n"))
             (exp1-res (compile-expression (cdddr tokens) st class-name))
             (exp1-vm (car exp1-res))
             (rem1 (cdr exp1-res)) ;; ']'
             (exp2-res (compile-expression (cddr rem1) st class-name)) ;; מדלגים על ] = 
             (exp2-vm (car exp2-res))
             (rem2 (cdr exp2-res)) ;; ';'
             (vm (string-append push-arr exp1-vm "add\n" exp2-vm "pop temp 0\npop pointer 1\npush temp 0\npop that 0\n")))
        (cons vm (cdr rem2)))
      ;; השמה למשתנה רגיל: let x = expr
      (let* ((exp-res (compile-expression (cdddr tokens) st class-name))
             (exp-vm (car exp-res))
             (rem (cdr exp-res)) ;; ';'
             (info (symbol-table-lookup st var-name))
             (pop-vm (string-append (write-pop (kind->segment (var-info-kind info)) (var-info-index info)) "\n")))
        (cons (string-append exp-vm pop-vm) (cdr rem)))))

(define (compile-do tokens st class-name)
  (define call-res (compile-subroutine-call (cadr tokens) (caddr tokens) (cdr tokens) st class-name))
  (define vm (string-append (car call-res) "pop temp 0\n")) ;; do מתעלם מהערך המוחזר
  (cons vm (cdr (cdr call-res)))) ;; מדלגים על ';'

(define (compile-return tokens st class-name)
  (define next-val (get-token-value (cadr tokens)))
  (if (string=? next-val ";")
      (cons "push constant 0\nreturn\n" (cddr tokens))
      (let* ((exp-res (compile-expression (cdr tokens) st class-name))
             (exp-vm (car exp-res))
             (rem (cdr exp-res)))
        (cons (string-append exp-vm "return\n") (cdr rem)))))

(define (compile-while tokens st class-name)
  (define label-start (generate-label "WHILE_EXP"))
  (define label-end (generate-label "WHILE_END"))
  (define exp-res (compile-expression (cddr tokens) st class-name)) ;; מדלגים על while (
  (define exp-vm (car exp-res))
  (define rem1 (cddr (cdr exp-res))) ;; מדלגים על ) {
  (define body-res (compile-statements rem1 st class-name))
  (define body-vm (car body-res))
  (define rem2 (cdr (cdr body-res))) ;; מדלגים על }
  (define vm (string-append (write-label label-start) "\n" exp-vm "not\n" (write-if-goto label-end) "\n"
                            body-vm (write-goto label-start) "\n" (write-label label-end) "\n"))
  (cons vm rem2))

(define (compile-if tokens st class-name)
  (define label-true (generate-label "IF_TRUE"))
  (define label-false (generate-label "IF_FALSE"))
  (define label-end (generate-label "IF_END"))
  (define exp-res (compile-expression (cddr tokens) st class-name)) ;; מדלגים על if (
  (define exp-vm (car exp-res))
  (define rem1 (cddr (cdr exp-res))) ;; מדלגים על ) {
  (define body-res (compile-statements rem1 st class-name))
  (define body-vm (car body-res))
  (define rem2 (cdr body-res)) ;; '}'
  
  (define next-val (if (null? (cdr rem2)) "" (get-token-value (cadr rem2))))
  (if (string=? next-val "else")
      (let* ((else-body-res (compile-statements (cdddr rem2) st class-name)) ;; מדלגים על else {
             (else-vm (car else-body-res))
             (rem3 (cdr (cdr else-body-res))) ;; '}'
             (vm (string-append exp-vm "if-goto " label-true "\ngoto " label-false "\n"
                                (write-label label-true) "\n" body-vm "goto " label-end "\n"
                                (write-label label-false) "\n" else-vm (write-label label-end) "\n")))
        (cons vm rem3))
      (let ((vm (string-append exp-vm "if-goto " label-true "\ngoto " label-false "\n"
                               (write-label label-true) "\n" body-vm (write-label label-false) "\n")))
        (cons vm (cdr rem2)))))

(define (compile-statements tokens st class-name)
  (define (parse-loop curr-tokens acc-vm)
    (if (null? curr-tokens)
        (cons acc-vm curr-tokens)
        (let ((next-val (get-token-value (car curr-tokens))))
          (cond
            [(string=? next-val "let")
             (define res (compile-let curr-tokens st class-name))
             (parse-loop (cdr res) (string-append acc-vm (car res)))]
            [(string=? next-val "do")
             (define res (compile-do curr-tokens st class-name))
             (parse-loop (cdr res) (string-append acc-vm (car res)))]
            [(string=? next-val "return")
             (define res (compile-return curr-tokens st class-name))
             (parse-loop (cdr res) (string-append acc-vm (car res)))]
            [(string=? next-val "while")
             (define res (compile-while curr-tokens st class-name))
             (parse-loop (cdr res) (string-append acc-vm (car res)))]
            [(string=? next-val "if")
             (define res (compile-if curr-tokens st class-name))
             (parse-loop (cdr res) (string-append acc-vm (car res)))]
            [else (cons acc-vm curr-tokens)]))))
  (parse-loop tokens ""))

;; =================================================================
;; פונקציות הקימפול של המחלקה והשגרות
;; =================================================================

(define (compile-subroutine tokens class-st class-name)
  (define sub-type (get-token-value (car tokens))) ;; constructor/function/method
  (define sub-name (get-token-value (caddr tokens)))
  
  (define st-sub (symbol-table-start-subroutine class-st))
  (define st-this (if (string=? sub-type "method") (symbol-table-add st-sub "this" class-name 'argument) st-sub))
  
  (define param-res (compile-parameter-list (cddddr tokens) st-this))
  (define st-params (car param-res))
  (define tokens-after-params (cddr (cdr param-res))) ;; מדלגים על ) ו- {
  
  (define (parse-vars curr-tokens curr-st)
    (if (string=? (get-token-value (car curr-tokens)) "var")
        (let ((res (compile-var-dec curr-tokens curr-st)))
          (parse-vars (cdr res) (car res)))
        (cons curr-st curr-tokens)))
        
  (define vars-res (parse-vars tokens-after-params st-params))
  (define st-final (car vars-res))
  (define start-statements (cdr vars-res))
  
  ;; כתיבת פקודת ה-function של ה-VM
  (define num-locals (symbol-table-var-count st-final 'local))
  (define vm-func (string-append (write-function (format "~a.~a" class-name sub-name) num-locals) "\n"))
  
  ;; התאמות ל-constructor ול-method
  (define vm-init
    (cond
      [(string=? sub-type "constructor")
       (let ((num-fields (symbol-table-var-count st-final 'field)))
         (string-append (write-push "constant" num-fields) "\ncall Memory.alloc 1\npop pointer 0\n"))]
      [(string=? sub-type "method")
       "push argument 0\npop pointer 0\n"]
      [else ""]))
      
  (define body-res (compile-statements start-statements st-final class-name))
  (define vm-body (car body-res))
  (define rem (cdr (cdr body-res))) ;; עוקפים את '}'
  
  (cons (string-append vm-func vm-init vm-body) rem))

(define (compile-class tokens class-st)
  (define class-name (get-token-value (cadr tokens)))
  (define start-tokens (cdddr tokens)) 
  
  (define (parse-class-body curr-tokens curr-st acc-vm)
    (define next-val (get-token-value (car curr-tokens)))
    (cond
      [(or (string=? next-val "static") (string=? next-val "field"))
       (define res (compile-class-var-dec curr-tokens curr-st))
       (parse-class-body (cdr res) (car res) acc-vm)]
      [(or (string=? next-val "constructor") (string=? next-val "function") (string=? next-val "method"))
       (define res (compile-subroutine curr-tokens curr-st class-name))
       (parse-class-body (cdr res) curr-st (string-append acc-vm (car res)))]
      [else acc-vm])) ;; סיום המחלקה
      
  (parse-class-body start-tokens class-st ""))

;; =================================================================
;; פונקציית הפעלה ראשית
;; =================================================================
(define (start-parsing jack-file-path)
  (define output-file-path (string-replace jack-file-path ".jack" ".vm"))
  (define tokens (map token->xml-string (tokenize jack-file-path)))
  (define vm-code (compile-class tokens (make-empty-symbol-table)))
  (display-to-file vm-code output-file-path #:exists 'replace)
  (displayln (format "SUCCESS! VM code successfully generated at:\n~a" output-file-path)))

;; =================================================================
;; סט בדיקות לפרויקט 11 - מחיקת נקודה-פסיק (;) תפעיל את הקימפול
;; =================================================================

;; --- 1. Seven ---
;(start-parsing "C:\\Users\\mayan\\Desktop\\עקרונות\\nand2tetris\\nand2tetris\\projects\\11\\Seven\\Main.jack")

;; --- 2. ConvertToBin ---
;(start-parsing "C:\\Users\\mayan\\Desktop\\עקרונות\\nand2tetris\\nand2tetris\\projects\\11\\ConvertToBin\\Main.jack")

;; --- 3. Average ---
;(start-parsing "C:\\Users\\mayan\\Desktop\\עקרונות\\nand2tetris\\nand2tetris\\projects\\11\\Average\\Main.jack")

;; --- 4. ComplexArrays ---
;(start-parsing "C:\\Users\\mayan\\Desktop\\עקרונות\\nand2tetris\\nand2tetris\\projects\\11\\ComplexArrays\\Main.jack")

;; --- 5. Square (יש לקמפל את כל ה-3 יחד) ---
;(start-parsing "C:\\Users\\mayan\\Desktop\\עקרונות\\nand2tetris\\nand2tetris\\projects\\11\\Square\\Main.jack")
;(start-parsing "C:\\Users\\mayan\\Desktop\\עקרונות\\nand2tetris\\nand2tetris\\projects\\11\\Square\\Square.jack")
;(start-parsing "C:\\Users\\mayan\\Desktop\\עקרונות\\nand2tetris\\nand2tetris\\projects\\11\\Square\\SquareGame.jack")

;; --- 6. Pong (יש לקמפל את כל ה-4 יחד) ---
;(start-parsing "C:\\Users\\mayan\\Desktop\\עקרונות\\nand2tetris\\nand2tetris\\projects\\11\\Pong\\Main.jack")
;(start-parsing "C:\\Users\\mayan\\Desktop\\עקרונות\\nand2tetris\\nand2tetris\\projects\\11\\Pong\\Bat.jack")
;(start-parsing "C:\\Users\\mayan\\Desktop\\עקרונות\\nand2tetris\\nand2tetris\\projects\\11\\Pong\\Ball.jack")
;(start-parsing "C:\\Users\\mayan\\Desktop\\עקרונות\\nand2tetris\\nand2tetris\\projects\\11\\Pong\\PongGame.jack")