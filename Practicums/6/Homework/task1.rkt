#lang racket

(define (meets-requirements x predicates)
  (cond
    [(null? predicates) #t]
    [(not ((car predicates) x)) #f]
    [else (meets-requirements x (cdr predicates))]
    )
  )

(define (where xs predicates)
  (define (helper result left-over)
    (cond
      [(null? left-over) result]
      [(meets-requirements (car left-over) predicates) (helper (append result (list (car left-over))) (cdr left-over))]
      [else (helper result (cdr left-over))]
      )
    )
  (helper '() xs)
  )

(equal? (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10)) ; all even numbers greater than 5
(equal? (where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) '()) ; no numbers are even and greater than 5
(equal? (where '() (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '())
(equal? (where '(1 2 3 4 5 6 7 8 9 10 11 13 15) (list odd? (λ (x) (> x 3)) (λ (x) (< x 20)))) '(5 7 9 11 13 15))