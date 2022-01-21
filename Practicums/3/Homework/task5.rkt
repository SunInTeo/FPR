#lang racket

(define (p n)
  (cond
    [(= n 1) 1]
    [else (+ (p (sub1 n)) (* 3 n) -2)] ;formula from wikipedia
    )
  )

(= (p 1) 1)
(= (p 2) 5)
(= (p 3) 12)
(= (p 4) 22)
(= (p 5) 35)
(= (p 6) 51)