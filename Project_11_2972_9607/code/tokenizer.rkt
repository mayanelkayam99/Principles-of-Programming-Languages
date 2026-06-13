#lang racket
(provide tokenize token->xml-string keywords) ; <-- השורה החדשה שפתחת החוצה את הפונקציות
;; =================================================================
;; 1. הגדרות וקבועים של שפת Jack
;; =================================================================

(define keywords
  '("class" "constructor" "function" "method" "field" "static" "var" "int" 
    "char" "boolean" "void" "true" "false" "null" "this" "let" "do" 
    "if" "else" "while" "return"))

;; =================================================================
;; שלב 1: ניקוי הערות שורה והערות בלוק מהקובץ
;; =================================================================
(define (remove-comments file-path)
  ;; קריאת כל השורות מהקובץ כרשימה של מחרוזות
  (define lines (file->lines file-path))
  
  ;; מעבר שורה-שורה וניקוי הערות שורה (//)
  (define clean-lines
    (map (lambda (line) (regexp-replace (regexp "//.*") line "")) lines))
  
  ;; חיבור כל השורות חזרה למחרוזת אחת גדולה עם ירידות שורה
  (define full-text (string-join clean-lines "\n"))
  
  ;; ניקוי הערות בלוק (/* ... */) בצורה עצלנית ויציבה
  (define text-no-blocks
    (regexp-replace* (regexp "/\\*(.|\\n)*?\\*/") full-text " "))
  
  text-no-blocks)

;; =================================================================
;; שלב 2: שאיבת כל הטוקנים החוקיים מהטקסט הנקי (גרסה מתוקנת לסוגריים מרובעים)
;; =================================================================
(define (tokenize file-path)
  (define clean-text (remove-comments file-path))
  
  ;; עדכנו את החלק של הסוגריים המרובעים ל- \\[ ו- \\] כדי שיתפסו בוודאות בכל גרסה
  (define token-regex 
    (regexp "\".*?\"|[0-9]+|[a-zA-Z_][a-zA-Z0-9_]*|\\{|\\}|\\(|\\)|\\[|\\]|\\.|,|;|\\+|\\-|\\*|/|&|\\||<|>|=|~"))
  
  (regexp-match* token-regex clean-text))

;; =================================================================
;; שלב 3: סיווג טוקנים והמרתם למבנה XML תקני
;; =================================================================
(define (token->xml-string token)
  (cond
    ;; א. stringConstant: מחרוזת קבועה (מנקים את הגרשיים העוטפים)
    [(and (string-prefix? token "\"") (string-suffix? token "\""))
     (define content (substring token 1 (- (string-length token) 1)))
     (format "<stringConstant> ~a </stringConstant>" content)]
    
    ;; ב. integerConstant: מספר שלם של ספרות בלבד
    [(regexp-match? (regexp "^[0-9]+$") token)
     (format "<integerConstant> ~a </integerConstant>" token)]
    
    ;; ג. keyword: מילה שמורה מתוך הרשימה המוגדרת
    [(member token keywords)
     (format "<keyword> ~a </keyword>" token)]
    
    ;; ד. symbol: סימבול חוקי בשפה (כולל המרה בטוחה לתווי XML מיוחדים)
    [(member token '("{" "}" "(" ")" "[" "]" "." "," ";" "+" "-" "*" "/" "&" "|" "<" ">" "=" "~"))
     (define safe-token
       (cond
         [(string=? token "<") "&lt;"]
         [(string=? token ">") "&gt;"]
         [(string=? token "&") "&amp;"]
         [(string=? token "\"") "&quot;"]
         [else token]))
     (format "<symbol> ~a </symbol>" safe-token)]
    
    ;; ה. identifier: מזהה (שם משתנה, פונקציה או קלאס)
    [else
     (format "<identifier> ~a </identifier>" token)]))

;; =================================================================
;; פונקציה ראשית: מעבדת את קובץ ה-Jack ומייצרת את קובץ ה-XML
;; =================================================================
(define (convert-to-xml-file jack-file-path output-file-path)
  ;; א. שאיבת רשימת הטוקנים מהקובץ
  (define tokens (tokenize jack-file-path))
  
  ;; ב. סיווג והפיכת כל טוקן לשורת XML עטופה בתגית שלו
  (define xml-lines (map token->xml-string tokens))
  
  ;; ג. הוספת תגיות הפתיחה והסגירה הגלובליות של קובץ הטוקנים
  (define full-xml 
    (string-join (append '("<tokens>") xml-lines '("</tokens>")) "\n"))
  
  ;; ד. כתיבה או דריסה של קובץ הפלט על הדיסק
  (display-to-file full-xml output-file-path #:exists 'replace))

;; =================================================================
;; שורת הרצה לבדיקת הפרויקט
;; =================================================================
;; הפונקציה קוראת את Square.jack ומייצרת את קובץ ה-XML הסופי: SquareT.xml
;(convert-to-xml-file "Main.jack" "MainT1.xml")
;(displayln "Success! Code compiled and 'SquareT.xml' has been generated.")