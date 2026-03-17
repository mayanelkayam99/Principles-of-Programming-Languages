#lang racket

(define total-buy 0)
(define total-cell 0)

(define (handle-buy product amount price out)
  (define total (* amount price))
  (fprintf out "### BUY ~a ###\n" product)
  (fprintf out "~a\n" (~r total #:precision '(= 2)))
  (set! total-buy (+ total-buy total)))

(define (handle-cell product amount price out)
  (define total (* amount price))
  (fprintf out "$$$ CELL ~a $$$\n" product)
  (fprintf out "~a\n" (~r total #:precision '(= 2)))
  (set! total-cell (+ total-cell total)))

(define dir-path "C:/Users/mayan/RacketProject/Tar0")
(define files
  (filter (lambda (f)
            (regexp-match? #rx"\\.vm$" (path->string f)))
          (directory-list dir-path)))

(define dir-name
  (last (string-split dir-path "/")))

(define output-file
  (string-append dir-path "/" dir-name ".asm"))

(call-with-output-file output-file
  (lambda (out)
    
    (for ([file files])
      
      (define file-name (path->string file))
      (define name (regexp-replace #rx"\\.vm$" file-name ""))
      
      (fprintf out "~a\n\n" name)
      
      (define in (open-input-file (build-path dir-path file)))
      
      (for ([line (in-lines in)])
        (define parts (string-split line))
        (define command (first parts))
        (define product (second parts))
        (define amount (string->number (third parts)))
        (define price (string->number (fourth parts)))
        
        (cond
          [(string=? command "buy")
           (handle-buy product amount price out)]
          
          [(string=? command "cell")
           (handle-cell product amount price out)]))
      
      (close-input-port in))
    
    (fprintf out "TOTAL BUY: ~a\n" (~r total-buy #:precision '(= 2)))
    (fprintf out "TOTAL CELL: ~a\n" (~r total-cell #:precision '(= 2))))
  
  #:exists 'replace)

(displayln (string-append "TOTAL BUY: " (~r total-buy #:precision '(= 2))))
(displayln (string-append "TOTAL CELL: " (~r total-cell #:precision '(= 2))))
