#lang racket

(require racket/trace)

(define (count-digits num)
  (if (< num 10)
      1
      (add1 (count-digits (quotient num 10)))
      )
  )

(define (count-zeroes n)
  (define (helper result counter)
    (cond
      [(> counter (count-digits n)) result]
      [else (helper (+ result (floor (/ n (expt 5 counter)))) (add1 counter))]
      )
    )
  (trace helper)
  (helper 0 1)
  )

(define (trailing-zeros n)
  (λ (p) (p (count-zeroes n)))
  )

((trailing-zeros 6) even?) 
((trailing-zeros 1000) even?)
((trailing-zeros 100000) even?)
((trailing-zeros 1000000000) even?)
