#lang racket
(require math/number-theory)

(define (sum-digits-rec n)
  (cond
    [(not (positive? n)) (error "The number is negative.")]
    [(< n 10) n]
    [else (+ (remainder n 10) (sum-digits-rec (quotient n 10)))]
    )
  )

(= (sum-digits-rec 123) 6)
(= (sum-digits-rec 12345) 15)