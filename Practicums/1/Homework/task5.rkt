#lang racket
(require racket/trace)

(define (snail column-h distance-d distance-n)
  (define (helper current-h number-of-days)
    (cond
      [(>= (+ current-h distance-d) column-h) (add1 number-of-days)]
      [else (helper (+ (- distance-d distance-n) current-h) (add1 number-of-days))]
      )
    )
  (helper 0 0)
  )

;(snail 3 2 1)
(= (snail 3 2 1) 2)
(= (snail 10 3 1) 5)
(= (snail 10 3 2) 8)
(= (snail 100 20 5) 7)
(= (snail 5 10 3) 1)