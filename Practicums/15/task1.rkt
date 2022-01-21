#lang racket

(define (decreasing? n)
  (if (< n 10)
      #t
      (and
       (<= (remainder n 10) (remainder (quotient n 10) 10))
       (decreasing? (quotient n 10))
       )
      )
  )

(define (sum-numbers a b)
  (cond
    [(> a b) 0]
    [(decreasing? a) (+ a (sum-numbers (add1 a) b))]
    [else (sum-numbers (add1 a) b)]
    )
  )

(define (sum-numbers-xs a b)
  (apply + (filter decreasing? (range a (add1 b))))
  )

(= (sum-numbers 1 9) 45)
(= (sum-numbers 199 203) 200)
(= (sum-numbers 219 225) 663)


(= (sum-numbers-xs 1 9) 45)
(= (sum-numbers-xs 199 203) 200)
(= (sum-numbers-xs 219 225) 663)
