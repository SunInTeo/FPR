#lang racket

(define (sum-digits-iter n)
  (define (helper current-n result)
    (cond
      [(< current-n 10) (+ result current-n)]
      [else (helper (quotient current-n 10) (+ result (remainder current-n 10)))]
      )
    )
  (cond
    [(< n 0) (error "n was negative")]
    [else (helper n 0)]
    )
  )

(= (sum-digits-iter 12345) 15)
(= (sum-digits-iter 123) 6)
; (sum-digits-iter -13) ; error "n was negative"