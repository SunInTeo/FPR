#lang racket

(require racket/trace)

(define (calc-series-sum x n)
  (define (helper result counter denominator)
    (cond
      [(> counter n) result]
      [else (helper
             (+ result (/ (* (expt -1 (add1 counter)) (expt 2 (add1 counter)) (expt x counter)) denominator))
             (add1 counter)
             (+ 2 denominator)
             )]
      )
    )
  (trace helper)
  (helper 0 0 1)
  )

(calc-series-sum 1 0) ; -2
(calc-series-sum 1 1) ; -2/3
(calc-series-sum 1 2) ; -1 1/5
(calc-series-sum 1 3) ; -1 1/21
(calc-series-sum 1 4) ; -1 11/135
(calc-series-sum 1 5) ; -1 29/385
(calc-series-sum 1 6) ; -1 937/12285