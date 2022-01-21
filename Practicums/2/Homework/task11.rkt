#lang racket

(require math/number-theory)

(define (nth-cubic num)
  (define (helper current-count subtractor)
    (cond
      [(and (= current-count num) (prime? (- (expt (add1 subtractor) 3) (expt subtractor 3)))) (- (expt (add1 subtractor) 3) (expt subtractor 3))]
      [(prime? (- (expt (add1 subtractor) 3) (expt subtractor 3))) (helper (add1 current-count) (add1 subtractor))]
      [else (helper current-count (add1 subtractor))]
      )
    )
  (cond
    [(> num 0) (helper 1 1)]
    [else (error "n should not be negative")]
    )
  )

(= (nth-cubic 1) 7)
(= (nth-cubic 4) 61)
(= (nth-cubic 50) 55897)
(= (nth-cubic 100) 283669)
(= (nth-cubic 200) 1570357)
; (nth-cubic 0) ; should return an error