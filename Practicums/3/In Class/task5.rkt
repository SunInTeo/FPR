#lang racket

(define (count-digits n)
  (if (< n 10)
      1
      (add1 (count-digits (quotient n 10)))
      )
  )

(define (get-last-n-digits num num-digits)
  (remainder num (expt 10 num-digits))
  )

(define (sub-num? x y)
  (define (helper left-over)
    (cond
      [(> x left-over) #f]
      [(= x (get-last-n-digits left-over (count-digits x))) #t]
      [else (helper (quotient left-over 10))]
      )
    )
  (if (and (not (negative? x)) (not(negative? y)))
      (helper y)
      (error "x and y must be non negative")
      )
  )


(equal? (sub-num? 123 5123783) #t)
(equal? (sub-num? 0 0) #t)
(equal? (sub-num? 10 101) #t)
(equal? (sub-num? 101 101) #t)
(equal? (sub-num? 10 0) #f)
(equal? (sub-num? 1253 5123783) #f)
(equal? (sub-num? 12 0) #f)