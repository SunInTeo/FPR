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

(define (interesting? num)
  (divides? (sum-of-digits num) num)
  )

(equal? (interesting? 410) #t)