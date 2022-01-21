#lang racket

(require math/number-theory)

(define (factorize num)
  (define (helper result left-over current-divisor)
    (cond
      [(= left-over 1) result]
      [(divides? current-divisor left-over) (helper (cons current-divisor result) (quotient left-over current-divisor)  current-divisor)]
      [else (helper result left-over (add1 current-divisor))]
      )
    )
  (reverse (helper '() num 2))
)


(equal? (factorize 2) '(2))
(equal? (factorize 6) '(2 3))
(equal? (factorize 13) '(13))
(equal? (factorize 123) '(3 41))
(equal? (factorize 152) '(2 2 2 19))
(equal? (factorize 12356498) '(2 7 11 19 41 103))