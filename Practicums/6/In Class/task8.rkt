#lang racket

(define (my-flatten xs)
  (define (helper result current-xs)
    (cond
      [(null? current-xs) result]
      [(list? (car current-xs)) (append (reverse (helper result (car current-xs))) (helper result (cdr current-xs)))]
      [else (helper (cons (car current-xs) result) (cdr current-xs))])
    )
  (helper '() xs)
  )

(equal? (my-flatten '((1 2 3) (4 5 6) ((7 8) (9 10 (11 (12)))))) '(1 2 3 4 5 6 7 8 9 10 11 12))