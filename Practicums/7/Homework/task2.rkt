#lang racket

(require math/number-theory)

(define (get-prime-divisors n)
  (filter (λ (x) (and (prime? x) (divides? x n))) (range 1 n))
  )

(define (return-list-of-numbers n k)
  (filter (λ (x) (< (length (get-prime-divisors x)) k)) (range 1 (add1 n)))
  )

(define (numbers n)
  (λ (k) (return-list-of-numbers n k))
  )

(equal? ((numbers 10) 1) '(1 2 3 5 7))
(equal? ((numbers 20) 2) '(1 2 3 4 5 7 8 9 11 13 16 17 19))