#lang racket

(define (count-digits n)
  (if (< n 10)
      1
      (add1 (count-digits (quotient n 10)))
      )
  )

(define (narcissistic? n)
  (define (helper left-over digits)
    (if (zero? left-over)
        0
        (+ (expt(remainder left-over 10) digits) (helper (quotient left-over 10) digits))
        )
    )
  (= (helper n (count-digits n)) n)
  )

(narcissistic? 7)
(equal? (narcissistic? 7) #t)
(equal? (narcissistic? 12) #f)
(equal? (narcissistic? 370) #t)
(equal? (narcissistic? 371) #t)
(equal? (narcissistic? 1634) #t)