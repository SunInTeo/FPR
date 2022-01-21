#lang racket

(define (in-decreasing-order? n)
  (define (helper left-over)
    (cond
      [(< left-over 10) #t]
      [(<= (remainder left-over 10) (remainder (quotient left-over 10) 10)) (helper (quotient left-over 10))]
      [else #f]
      )
    )
  (helper n)
  )

(define (sum-numbers start finish)
  (define (helper  current-n)
    (cond
      [(> current-n finish) 0]
      [(in-decreasing-order? current-n) (+ current-n (helper (add1 current-n)))]
      [else (helper (add1 current-n))]
      )
    )
  (if (> start finish)
      (sum-numbers finish start)
      (helper start)
      )
  )

(= (sum-numbers 1 9) 45)
(= (sum-numbers 199 203) 200)
(= (sum-numbers 219 225) 663)
(= (sum-numbers 225 219) 663)