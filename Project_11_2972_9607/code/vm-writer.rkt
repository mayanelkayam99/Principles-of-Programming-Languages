#lang racket

(provide (all-defined-out))

;; 1. פקודות דחיפה ושליפה מהמחסנית
(define (write-push segment index)
  ;; segment: string (e.g., "local", "constant")
  ;; index: integer
  (format "push ~a ~a" segment index))

(define (write-pop segment index)
  ;; segment: string (e.g., "argument", "this")
  ;; index: integer
  (format "pop ~a ~a" segment index))

;; 2. פקודות אריתמטיות ולוגיות
(define (write-arithmetic command)
  ;; command: string (e.g., "add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not")
  (format "~a" command))

;; 3. פקודות ניהול תוויות וזרימת קוד
(define (write-label label)
  ;; label: string
  (format "label ~a" label))

(define (write-goto label)
  ;; label: string
  (format "goto ~a" label))

(define (write-if-goto label)
  ;; label: string
  (format "if-goto ~a" label))

;; 4. פקודות ניהול פונקציות ומתודות
(define (write-call name n-args)
  ;; name: string, n-args: integer
  (format "call ~a ~a" name n-args))

(define (write-function name n-locals)
  ;; name: string, n-locals: integer
  (format "function ~a ~a" name n-locals))

(define (write-return)
  "return")