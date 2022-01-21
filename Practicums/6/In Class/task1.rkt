#lang racket

(define (my-reverse-iter xs)
  (define (helper result current-xs)
    (cond
      [(null? current-xs) result]
      [else (helper (cons (car current-xs) result) (cdr current-xs))])
    )
  (helper '() xs)
)

(equal? (my-reverse-iter '(1 2 3 4 5)) '(5 4 3 2 1))