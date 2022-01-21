#lang racket

(define (procedure f y)
  (λ (x)
    (if (>= x (f x))
        y
        (f x)
        )
    )
  )

(= ((procedure (λ (x) (* 2 x)) 100) 50) 100)
(= ((procedure (λ (x) (* 2 x)) 100.236) 500.002) 1000.004)
(= ((procedure identity 1.001) 1.001) 1.001)