#lang racket

(require math/number-theory)

(define (sum-digit-divisors n)
  (define (helper current)
    (cond
      [(zero? current) 0]
      [(divides? (remainder current 10) n) (+ (remainder current 10) (helper (quotient current 10)))]
      [else (helper (quotient current 10))]
      )
    )
  (if (> n 0)
      (helper n)
      (error "n should be natural")
      )
  )

(= (sum-digit-divisors 1) 1)
(= (sum-digit-divisors 28) 2)
(= (sum-digit-divisors 32) 2)
(= (sum-digit-divisors 29) 0)
(= (sum-digit-divisors 34) 0)
(= (sum-digit-divisors 1048) 13)