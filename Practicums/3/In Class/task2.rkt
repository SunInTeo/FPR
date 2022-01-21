#lang racket

(define (calculate-sum x n)
  (define (helper sum left-over)
    (if (zero? left-over)
        (add1 sum)
        (helper (+ sum (expt x left-over)) (sub1 left-over))
        )
    )
  (helper 0 n)
  )

(= (calculate-sum 5 0) 1)
(= (calculate-sum 5 1) 6)
(= (calculate-sum 10 1) 11)
(= (calculate-sum 1 11) 12)
(= (calculate-sum 2 11) 4095)