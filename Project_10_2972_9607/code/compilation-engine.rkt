#lang racket
(require racket/string)
;; טעינת הפונקציות מהטוקנייזר (וודאי ששני הקבצים נמצאים באותה התיקייה)
(require "tokenizer.rkt") 

;; =================================================================
;; פונקציות עזר של מנוע הקימפול
;; =================================================================

;; 1. ייצור שורת XML עם הזחה מתאימה
(define (format-xml-line indent-level xml-content)
  (define spaces (make-string (* indent-level 2) #\space))
  (string-append spaces xml-content))

;; 2. שליפת הטוקן הגולמי מתוך מחרוזת ה-XML של הטוקנייזר
(define (get-token-value xml-token-string)
  (define match (regexp-match #px"<[^>]+>\\s*(.*?)\\s*</[^>]+>" xml-token-string))
  (if match
      (cadr match)
      ""))

;; 3. בדיקה האם הטוקן הוא אופרטור מתמטי/לוגי
(define (is-op? token-val)
  (member token-val '("+" "-" "*" "/" "&amp;" "|" "&lt;" "&gt;" "=" "<" ">" "&")))

;; =================================================================
;; פונקציות הקימפול של ביטויים (Expressions & Terms)
;; =================================================================

;; compile-expression-list: מטפלת ברשימת פרמטרים שמועברים לפונקציה, מופרדים בפסיקים
(define (compile-expression-list tokens indent-level)
  (define open-tag (format-xml-line indent-level "<expressionList>"))
  
  (define (collect-exprs current-tokens accum-xml)
    (define first-val (get-token-value (car current-tokens)))
    (if (string=? first-val ")")
        (cons accum-xml current-tokens)
        (let* ((exp-res (compile-expression current-tokens (+ indent-level 1)))
               (exp-xml (car exp-res))
               (rem-after-exp (cdr exp-res)))
          (if (string=? (get-token-value (car rem-after-exp)) ",")
              (let ((line-comma (format-xml-line (+ indent-level 1) (car rem-after-exp))))
                (collect-exprs (cdr rem-after-exp) (append accum-xml exp-xml (list line-comma))))
              (cons (append accum-xml exp-xml) rem-after-exp)))))
              
  (define res (collect-exprs tokens '()))
  (define close-tag (format-xml-line indent-level "</expressionList>"))
  (cons (append (list open-tag) (car res) (list close-tag)) (cdr res)))

;; compile-subroutine-call: פונקציית עזר לזיהוי וקריאה לפונקציות (למשל Array.new או readInt)
(define (compile-subroutine-call tokens indent-level)
  (define first-token (car tokens))
  (define next-val (get-token-value (cadr tokens)))
  
  (if (string=? next-val ".")
      (let* ((line-id (format-xml-line indent-level first-token))
             (line-dot (format-xml-line indent-level (cadr tokens)))
             (line-func (format-xml-line indent-level (caddr tokens)))
             (line-open (format-xml-line indent-level (cadddr tokens)))
             (explist-res (compile-expression-list (cddddr tokens) indent-level))
             (explist-xml (car explist-res))
             (rem-after-list (cdr explist-res))
             (line-close (format-xml-line indent-level (car rem-after-list))))
        (cons (append (list line-id line-dot line-func line-open) explist-xml (list line-close)) (cdr rem-after-list)))
      (let* ((line-func (format-xml-line indent-level first-token))
             (line-open (format-xml-line indent-level (cadr tokens)))
             (explist-res (compile-expression-list (cddr tokens) indent-level))
             (explist-xml (car explist-res))
             (rem-after-list (cdr explist-res))
             (line-close (format-xml-line indent-level (car rem-after-list))))
        (cons (append (list line-func line-open) explist-xml (list line-close)) (cdr rem-after-list)))))

;; compile-term: מטפלת באיבר בודד (מספר, מחרוזת, שם משתנה, קריאה לפונקציה, מערך או ביטוי בסוגריים)
(define (compile-term tokens indent-level)
  (define open-tag (format-xml-line indent-level "<term>"))
  (define first-token (car tokens))
  (define first-val (get-token-value first-token))
  
  (cond
    ;; מקרה 1: אופרטור אונרי (~ או -)
    [(or (string=? first-val "-") (string=? first-val "~"))
     (let* ((line-op (format-xml-line (+ indent-level 1) first-token))
            (term-res (compile-term (cdr tokens) (+ indent-level 1)))
            (term-xml (car term-res))
            (rem (cdr term-res)))
       (cons (append (list open-tag line-op) term-xml (list (format-xml-line indent-level "</term>"))) rem))]
       
    ;; מקרה 2: ביטוי בתוך סוגריים ( expression )
    [(string=? first-val "(")
     (let* ((line-open (format-xml-line (+ indent-level 1) first-token))
            (exp-res (compile-expression (cdr tokens) (+ indent-level 1)))
            (exp-xml (car exp-res))
            (rem-after-exp (cdr exp-res))
            (line-close (format-xml-line (+ indent-level 1) (car rem-after-exp)))
            (rem (cdr rem-after-exp)))
       (cons (append (list open-tag line-open) exp-xml (list line-close (format-xml-line indent-level "</term>"))) rem))]
       
    ;; מקרה 3: קבועים (מספרים, מחרוזות, מילים שמורות)
    [(or (string-contains? first-token "<integerConstant>") 
         (string-contains? first-token "<stringConstant>")
         (string-contains? first-token "<keyword>"))
     (cons (list open-tag (format-xml-line (+ indent-level 1) first-token) (format-xml-line indent-level "</term>")) 
           (cdr tokens))]
           
    ;; מקרה 4: מזהה (משתנה רגיל, גישה למערך או קריאה לפונקציה)
    [(string-contains? first-token "<identifier>")
     (let ((next-val (if (null? (cdr tokens)) "" (get-token-value (cadr tokens)))))
       (cond
         ;; גישה למערך: a[i]
         [(string=? next-val "[")
          (let* ((line-id (format-xml-line (+ indent-level 1) first-token))
                 (line-bracket (format-xml-line (+ indent-level 1) (cadr tokens)))
                 (exp-res (compile-expression (cddr tokens) (+ indent-level 1)))
                 (exp-xml (car exp-res))
                 (rem-after-exp (cdr exp-res))
                 (line-close-bracket (format-xml-line (+ indent-level 1) (car rem-after-exp)))
                 (rem (cdr rem-after-exp)))
            (cons (append (list open-tag line-id line-bracket) exp-xml (list line-close-bracket (format-xml-line indent-level "</term>"))) rem))]
            
         ;; קריאה לפונקציה
         [(or (string=? next-val "(") (string=? next-val "."))
          (let* ((call-res (compile-subroutine-call tokens (+ indent-level 1)))
                 (call-xml (car call-res))
                 (rem (cdr call-res)))
            (cons (append (list open-tag) call-xml (list (format-xml-line indent-level "</term>"))) rem))]
            
         ;; משתנה פשוט
         [else
          (cons (list open-tag (format-xml-line (+ indent-level 1) first-token) (format-xml-line indent-level "</term>")) 
                (cdr tokens))]))]
                
    [else (cons (list open-tag (format-xml-line (+ indent-level 1) first-token) (format-xml-line indent-level "</term>")) (cdr tokens))]))

;; compile-expression: מנתחת ביטוי מורכב שיכול להכיל איברים ואופרטורים (term op term)
(define (compile-expression tokens indent-level)
  (define open-tag (format-xml-line indent-level "<expression>"))
  
  (define term-res (compile-term tokens (+ indent-level 1)))
  (define term-xml (car term-res))
  (define tokens-after-term (cdr term-res))
  
  (define (collect-ops current-tokens accum-xml)
    (if (null? current-tokens)
        (cons accum-xml current-tokens)
        (let ((next-val (get-token-value (car current-tokens))))
          (if (is-op? next-val)
              (let* ((op-xml (list (format-xml-line (+ indent-level 1) (car current-tokens))))
                     (next-term-res (compile-term (cdr current-tokens) (+ indent-level 1)))
                     (next-term-xml (car next-term-res))
                     (rem-tokens (cdr next-term-res)))
                (collect-ops rem-tokens (append accum-xml op-xml next-term-xml)))
              (cons accum-xml current-tokens)))))
              
  (define ops-res (collect-ops tokens-after-term '()))
  (define ops-xml (car ops-res))
  (define rem (cdr ops-res))
  
  (define close-tag (format-xml-line indent-level "</expression>"))
  (cons (append (list open-tag) term-xml ops-xml (list close-tag)) rem))

;; =================================================================
;; פונקציות הקימפול של פקודות (Statements)
;; =================================================================

(define (compile-let tokens indent-level)
  (define open-tag (format-xml-line indent-level "<letStatement>"))
  (define t-let (car tokens))
  (define t-var (cadr tokens))
  (define line-let (format-xml-line (+ indent-level 1) t-let))
  (define line-var (format-xml-line (+ indent-level 1) t-var))
  
  (define next-val (get-token-value (caddr tokens)))
  (define is-array? (string=? next-val "["))
  
  (define after-var-tokens
    (if is-array?
        (let* ((line-open (format-xml-line (+ indent-level 1) (caddr tokens)))
               (exp-res (compile-expression (cdddr tokens) (+ indent-level 1)))
               (exp-xml (car exp-res))
               (rem (cdr exp-res))
               (line-close (format-xml-line (+ indent-level 1) (car rem))))
          (cons (append (list line-open) exp-xml (list line-close)) (cdr rem)))
        (cons '() (cddr tokens))))
        
  (define arr-xml (car after-var-tokens))
  (define tokens-at-eq (cdr after-var-tokens))
  
  (define line-eq (format-xml-line (+ indent-level 1) (car tokens-at-eq)))
  
  (define exp-res2 (compile-expression (cdr tokens-at-eq) (+ indent-level 1)))
  (define exp-xml2 (car exp-res2))
  (define rem-after-exp (cdr exp-res2))
  
  (define line-semi (format-xml-line (+ indent-level 1) (car rem-after-exp)))
  (define close-tag (format-xml-line indent-level "</letStatement>"))
  
  (cons (append (list open-tag line-let line-var) arr-xml (list line-eq) exp-xml2 (list line-semi close-tag)) (cdr rem-after-exp)))

(define (compile-do tokens indent-level)
  (define open-tag (format-xml-line indent-level "<doStatement>"))
  (define line-do (format-xml-line (+ indent-level 1) (car tokens)))
  
  (define call-res (compile-subroutine-call (cdr tokens) (+ indent-level 1)))
  (define call-xml (car call-res))
  (define rem (cdr call-res))
  
  (define line-semi (format-xml-line (+ indent-level 1) (car rem)))
  (define close-tag (format-xml-line indent-level "</doStatement>"))
  (cons (append (list open-tag line-do) call-xml (list line-semi close-tag)) (cdr rem)))

(define (compile-return tokens indent-level)
  (define open-tag (format-xml-line indent-level "<returnStatement>"))
  (define line-return (format-xml-line (+ indent-level 1) (car tokens)))
  
  (define next-val (get-token-value (cadr tokens)))
  
  (if (string=? next-val ";")
      (let ((line-semi (format-xml-line (+ indent-level 1) (cadr tokens))))
        (cons (list open-tag line-return line-semi (format-xml-line indent-level "</returnStatement>"))
              (cddr tokens)))
      (let* ((exp-res (compile-expression (cdr tokens) (+ indent-level 1)))
             (exp-xml (car exp-res))
             (rem (cdr exp-res))
             (line-semi (format-xml-line (+ indent-level 1) (car rem))))
        (cons (append (list open-tag line-return) exp-xml (list line-semi (format-xml-line indent-level "</returnStatement>")))
              (cdr rem)))))

(define (compile-while tokens indent-level)
  (define open-tag (format-xml-line indent-level "<whileStatement>"))
  (define t-while (car tokens))
  (define t-open-paren (cadr tokens))
  (define line-while (format-xml-line (+ indent-level 1) t-while))
  (define line-open-paren (format-xml-line (+ indent-level 1) t-open-paren))
  
  (define exp-res (compile-expression (cddr tokens) (+ indent-level 1)))
  (define exp-xml (car exp-res))
  (define rem-after-exp (cdr exp-res))
  
  (define t-close-paren (car rem-after-exp))
  (define t-open-brace (cadr rem-after-exp))
  (define line-close-paren (format-xml-line (+ indent-level 1) t-close-paren))
  (define line-open-brace (format-xml-line (+ indent-level 1) t-open-brace))
  
  (define tokens-inside-body (cddr rem-after-exp))
  (define body-res (compile-statements tokens-inside-body (+ indent-level 1)))
  (define body-xml (car body-res))
  (define tokens-after-body (cdr body-res))
  
  (define t-close-brace (car tokens-after-body))
  (define line-close-brace (format-xml-line (+ indent-level 1) t-close-brace))
  (define close-tag (format-xml-line indent-level "</whileStatement>"))
  
  (cons (append (list open-tag line-while line-open-paren) exp-xml (list line-close-paren line-open-brace) body-xml (list line-close-brace close-tag))
        (cdr tokens-after-body)))

(define (compile-if tokens indent-level)
  (define open-tag (format-xml-line indent-level "<ifStatement>"))
  (define t-if (car tokens))
  (define t-open-paren (cadr tokens))
  (define line-if (format-xml-line (+ indent-level 1) t-if))
  (define line-open-paren (format-xml-line (+ indent-level 1) t-open-paren))
  
  (define exp-res (compile-expression (cddr tokens) (+ indent-level 1)))
  (define exp-xml (car exp-res))
  (define rem1 (cdr exp-res))
  
  (define t-close-paren (car rem1))
  (define t-open-brace (cadr rem1))
  (define line-close-paren (format-xml-line (+ indent-level 1) t-close-paren))
  (define line-open-brace (format-xml-line (+ indent-level 1) t-open-brace))
  
  (define body-res (compile-statements (cddr rem1) (+ indent-level 1)))
  (define body-xml (car body-res))
  (define rem2 (cdr body-res))
  
  (define t-close-brace (car rem2))
  (define line-close-brace (format-xml-line (+ indent-level 1) t-close-brace))
  
  (define rem3 (cdr rem2))
  (if (and (not (null? rem3)) (string=? (get-token-value (car rem3)) "else"))
      (let* ((t-else (car rem3))
             (t-open-b2 (cadr rem3))
             (line-else (format-xml-line (+ indent-level 1) t-else))
             (line-open-b2 (format-xml-line (+ indent-level 1) t-open-b2))
             (body2-res (compile-statements (cddr rem3) (+ indent-level 1)))
             (body2-xml (car body2-res))
             (rem4 (cdr body2-res))
             (t-close-b2 (car rem4))
             (line-close-b2 (format-xml-line (+ indent-level 1) t-close-b2)))
        (cons (append (list open-tag line-if line-open-paren) exp-xml 
                      (list line-close-paren line-open-brace) body-xml (list line-close-brace)
                      (list line-else line-open-b2) body2-xml (list line-close-b2)
                      (list (format-xml-line indent-level "</ifStatement>")))
              (cdr rem4)))
      (cons (append (list open-tag line-if line-open-paren) exp-xml 
                    (list line-close-paren line-open-brace) body-xml (list line-close-brace)
                    (list (format-xml-line indent-level "</ifStatement>")))
            rem3)))

(define (compile-statements tokens indent-level)
  (define open-tag (format-xml-line indent-level "<statements>"))
  
  (define (parse-loop current-tokens accum-xml)
    (if (null? current-tokens)
        (cons accum-xml current-tokens)
        (let* ((next-token (car current-tokens))
               (next-val (get-token-value next-token)))
          (cond
            [(string=? next-val "let")
             (define res (compile-let current-tokens (+ indent-level 1)))
             (parse-loop (cdr res) (append accum-xml (car res)))]
            [(string=? next-val "do")
             (define res (compile-do current-tokens (+ indent-level 1)))
             (parse-loop (cdr res) (append accum-xml (car res)))]
            [(string=? next-val "return")
             (define res (compile-return current-tokens (+ indent-level 1)))
             (parse-loop (cdr res) (append accum-xml (car res)))]
            [(string=? next-val "while")
             (define res (compile-while current-tokens (+ indent-level 1)))
             (parse-loop (cdr res) (append accum-xml (car res)))]
            [(string=? next-val "if")
             (define res (compile-if current-tokens (+ indent-level 1)))
             (parse-loop (cdr res) (append accum-xml (car res)))]
            [else 
             (cons accum-xml current-tokens)]))))
             
  (define loop-res (parse-loop tokens '()))
  (define close-tag (format-xml-line indent-level "</statements>"))
  (cons (append (list open-tag) (car loop-res) (list close-tag)) (cdr loop-res)))

;; =================================================================
;; פונקציות הקימפול של חלקי ה-Class
;; =================================================================

(define (compile-class-var-dec tokens indent-level)
  (define open-tag (format-xml-line indent-level "<classVarDec>"))
  (define (collect-until-semi current-tokens acc)
    (define current-token (car current-tokens))
    (define token-val (get-token-value current-token))
    (define new-acc (append acc (list (format-xml-line (+ indent-level 1) current-token))))
    (if (string=? token-val ";")
        (cons new-acc (cdr current-tokens))
        (collect-until-semi (cdr current-tokens) new-acc)))
  (define parse-result (collect-until-semi tokens '()))
  (define xml-lines (car parse-result))
  (define remaining (cdr parse-result))
  (define close-tag (format-xml-line indent-level "</classVarDec>"))
  (cons (append (list open-tag) xml-lines (list close-tag)) remaining))

(define (compile-parameter-list tokens indent-level)
  (define open-tag (format-xml-line indent-level "<parameterList>"))
  (define (collect-params current-tokens acc)
    (define current-token (car current-tokens))
    (define token-val (get-token-value current-token))
    (if (string=? token-val ")")
        (cons acc current-tokens)
        (collect-params (cdr current-tokens) 
                        (append acc (list (format-xml-line (+ indent-level 1) current-token))))))
  (define parse-result (collect-params tokens '()))
  (define xml-lines (car parse-result))
  (define remaining (cdr parse-result))
  (define close-tag (format-xml-line indent-level "</parameterList>"))
  (cons (append (list open-tag) xml-lines (list close-tag)) remaining))

(define (compile-var-dec tokens indent-level)
  (define open-tag (format-xml-line indent-level "<varDec>"))
  (define (collect-until-semi current-tokens acc)
    (define current-token (car current-tokens))
    (define token-val (get-token-value current-token))
    (define new-acc (append acc (list (format-xml-line (+ indent-level 1) current-token))))
    (if (string=? token-val ";")
        (cons new-acc (cdr current-tokens))
        (collect-until-semi (cdr current-tokens) new-acc)))
  (define parse-result (collect-until-semi tokens '()))
  (define xml-lines (car parse-result))
  (define remaining (cdr parse-result))
  (define close-tag (format-xml-line indent-level "</varDec>"))
  (cons (append (list open-tag) xml-lines (list close-tag)) remaining))

(define (compile-subroutine tokens indent-level)
  (define open-tag (format-xml-line indent-level "<subroutineDec>"))
  (define line1 (format-xml-line (+ indent-level 1) (car tokens)))
  (define line2 (format-xml-line (+ indent-level 1) (cadr tokens)))
  (define line3 (format-xml-line (+ indent-level 1) (caddr tokens)))
  (define line4 (format-xml-line (+ indent-level 1) (cadddr tokens)))
  
  (define param-res (compile-parameter-list (cddddr tokens) (+ indent-level 1)))
  (define param-xml (car param-res))
  (define tokens-after-params (cdr param-res))
  
  (define line-paren (format-xml-line (+ indent-level 1) (car tokens-after-params)))
  (define tokens-body (cdr tokens-after-params))
  (define line-body-open (format-xml-line (+ indent-level 2) (car tokens-body)))
  (define start-body-tokens (cdr tokens-body))
  
  (define (parse-vars current-tokens accum-xml)
    (define next-val (get-token-value (car current-tokens)))
    (if (string=? next-val "var")
        (let ((res (compile-var-dec current-tokens (+ indent-level 2))))
          (parse-vars (cdr res) (append accum-xml (car res))))
        (cons accum-xml current-tokens)))
        
  (define vars-res (parse-vars start-body-tokens '()))
  (define vars-xml (car vars-res))
  (define tokens-after-vars (cdr vars-res))
  
  (define statements-res (compile-statements tokens-after-vars (+ indent-level 2)))
  (define statements-xml (car statements-res))
  (define tokens-after-statements (cdr statements-res))
  
  (define line-body-close (format-xml-line (+ indent-level 2) (car tokens-after-statements)))
  (define body-open-tag (format-xml-line (+ indent-level 1) "<subroutineBody>"))
  (define body-close-tag (format-xml-line (+ indent-level 1) "</subroutineBody>"))
  (define close-tag (format-xml-line indent-level "</subroutineDec>"))
  
  (define full-xml (append (list open-tag line1 line2 line3 line4)
                           param-xml
                           (list line-paren body-open-tag line-body-open)
                           vars-xml
                           statements-xml
                           (list line-body-close body-close-tag close-tag)))
  (cons full-xml (cdr tokens-after-statements)))

(define (compile-class tokens indent-level)
  (define open-tag (format-xml-line indent-level "<class>"))
  (define line1 (format-xml-line (+ indent-level 1) (car tokens)))
  (define line2 (format-xml-line (+ indent-level 1) (cadr tokens)))
  (define line3 (format-xml-line (+ indent-level 1) (caddr tokens)))
  
  (define start-tokens (cdddr tokens)) 
  
  (define (parse-class-body current-tokens accum-xml)
    (define next-val (get-token-value (car current-tokens)))
    (cond
      [(or (string=? next-val "static") (string=? next-val "field"))
       (define res (compile-class-var-dec current-tokens (+ indent-level 1)))
       (parse-class-body (cdr res) (append accum-xml (car res)))]
      [(or (string=? next-val "constructor") (string=? next-val "function") (string=? next-val "method"))
       (define res (compile-subroutine current-tokens (+ indent-level 1)))
       (parse-class-body (cdr res) (append accum-xml (car res)))]
      [else
       (define line-close (format-xml-line (+ indent-level 1) (car current-tokens)))
       (define close-tag (format-xml-line indent-level "</class>"))
       (cons (append (list open-tag line1 line2 line3) accum-xml (list line-close close-tag)) 
             (cdr current-tokens))]))
  
  (parse-class-body start-tokens '()))

;; =================================================================
;; פונקציית הפעלה ראשית
;; =================================================================
(define (start-parsing jack-file-path output-file-path)
  (define tokens (map token->xml-string (tokenize jack-file-path)))
  (define result (compile-class tokens 0))
  (define final-xml-lines (car result))
  (display-to-file (string-join final-xml-lines "\n") output-file-path #:exists 'replace)
  (displayln (format "Success! ~a has been parsed into ~a" jack-file-path output-file-path)))

;; =================================================================
;; פונקציה לריצה אוטומטית על תיקייה (עם דריסת קבצים קיימים)
;; =================================================================
(define (compile-directory target-dir)
  ;; עוברים על כל הקבצים בתיקייה שסופקה
  (for ([file (directory-list target-dir)])
    (define file-str (path->string file))
    ;; בודקים אם הקובץ מסתיים ב-.jack
    (when (string-suffix? file-str ".jack")
      ;; מרכיבים את הנתיב המלא לקובץ הקלט
      (define jack-path (build-path target-dir file-str))
      ;; מחליפים את הסיומת ל-.xml ומרכיבים את הנתיב לקובץ הפלט באותה התיקייה
      (define xml-path (build-path target-dir (string-replace file-str ".jack" ".xml")))
      ;; מריצים את הקימפול (ידרוס את קובץ ה-XML הקיים)
      (start-parsing (path->string jack-path) (path->string xml-path)))))

;; הרצת הפונקציה על התיקייה המבוקשת
;; (שימי לב לעדכן את הנתיב לשם של התיקייה המשוכפלת שלך, למשל nand2tetris_copy)
(compile-directory 
 "C:\\Users\\micha\\OneDrive\\שולחן העבודה\\שנה ד\\סמסטר ב\\עקרונות\\nand2tetris\\projects\\10\\Square")