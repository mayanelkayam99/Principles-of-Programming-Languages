#lang racket

;; הגדרת הפונקציות והמבנים שיהיו נגישים לקבצים אחרים (כמו ה-Parser)
(provide (struct-out var-info)
         (struct-out symbol-table)
         make-empty-symbol-table
         symbol-table-add
         symbol-table-start-subroutine
         symbol-table-lookup
         symbol-table-var-count) ;; <--- השורה שהתווספה

;; ייצוג של משתנה בודד בטבלה
(struct var-info (type kind index) #:transparent)

;; ייצוג של טבלת הסמלים המלאה הכוללת את שני הסקופים והמונים
(struct symbol-table (class-table subroutine-table counters) #:transparent)

;; counters מחזיק את האינדקס הבא הפנוי לכל סוג משתנה
(define empty-counters
  #hash((static . 0) (field . 0) (argument . 0) (local . 0)))

;; יצירת טבלת סמלים ריקה לחלוטין (בתחילת קומפילציית המחלקה)
(define (make-empty-symbol-table)
  (symbol-table #hash() #hash() empty-counters))

;; הוספת משתנה חדש לטבלה והחזרת טבלה מעודכנת
(define (symbol-table-add st name type kind)
  (let* ([counters (symbol-table-counters st)]
         [current-index (hash-ref counters kind)]
         [new-info (var-info type kind current-index)]
         [new-counters (hash-set counters kind (+ current-index 1))])
    
    (cond
      ;; משתני מחלקה (static / field) נכנסים ל-class-table
      [(or (eq? kind 'static) (eq? kind 'field))
       (struct-copy symbol-table st
                    [class-table (hash-set (symbol-table-class-table st) name new-info)]
                    [counters new-counters])]
      
      ;; משתני תת-שגרה (argument / local) נכנסים ל-subroutine-table
      [(or (eq? kind 'argument) (eq? kind 'local))
       (struct-copy symbol-table st
                    [subroutine-table (hash-set (symbol-table-subroutine-table st) name new-info)]
                    [counters new-counters])])))

;; איפוס הסקופ המקומי בכניסה לפונקציה/מתודה חדשה
(define (symbol-table-start-subroutine st)
  (let* ([old-counters (symbol-table-counters st)]
         [new-counters (hash-set (hash-set old-counters 'argument 0) 'local 0)])
    (struct-copy symbol-table st
                 [subroutine-table #hash()]
                 [counters new-counters])))

;; חיפוש משתנה בטבלה (מחזיר var-info או #f אם לא נמצא)
(define (symbol-table-lookup st name)
  (hash-ref (symbol-table-subroutine-table st) name
            (lambda () 
              (hash-ref (symbol-table-class-table st) name #f))))

;; ספירת מספר המשתנים מסוג מסוים (למשל 'local או 'field) -- הפונקציה שהתווספה
(define (symbol-table-var-count st kind)
  (hash-ref (symbol-table-counters st) kind 0))