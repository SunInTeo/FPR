#lang racket

(require math/number-theory)

(define (sum-of-digits num)
  (define (helper left-over sum)
    (cond
      [(zero? left-over) sum]
      [else (helper (quotient left-over 10) (+ sum (remainder left-over 10)))]
      )   
    )
  (helper num 0)
  )

(define (sum-divisible-numbers start finish k)
  (cond
    [(> start finish) (sum-divisible-numbers finish start k)]
    [(= start finish)
     (if (divides? k (sum-of-digits start))
         (sum-of-digits start)
         0
         )
     ]
    [(divides? k (sum-of-digits start)) (+ start (sum-divisible-numbers (add1 start) finish k))]
    [else (sum-divisible-numbers (add1 start) finish k)]
    )
  )

(= (sum-divisible-numbers 0 10 5) 5)
(= (sum-divisible-numbers 0 100 5) 990)
(= (sum-divisible-numbers 100 0 5) 990)