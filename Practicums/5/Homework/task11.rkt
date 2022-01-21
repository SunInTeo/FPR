#lang racket

(define (concat-proc xs1 xs2)
  (append xs1 xs2)
  )

(define (concat-rec xs1 xs2)
  (cond
    [(null? xs1) xs2]
    [(null? xs1) xs1]
    [else (cons (car xs1) (concat-rec (cdr xs1) xs2))])
  )

; using a predefined procedure
(equal? (concat-proc '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

; using a linearly recursive process
(equal? (concat-rec '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))