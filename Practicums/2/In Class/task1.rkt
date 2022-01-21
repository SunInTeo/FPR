#lang racket

(define (rev n)
  (define (helper result left-over)
    (if (zero? left-over)
        result
        (helper  (+ (* result 10) (remainder left-over 10)) (quotient left-over 10))
    ))
  (helper 0 n)
  )
  

(define (palindrome? n)
  (= n (rev n))
  )


(equal? (palindrome? 1) #t)
(equal? (palindrome? 6) #t)
(equal? (palindrome? 1010) #f)
(equal? (palindrome? 505) #t)
(equal? (palindrome? 123321) #t)
(equal? (palindrome? 654) #f)
(equal? (palindrome? 121) #t)
(equal? (palindrome? 12) #f)
(equal? (palindrome? 120) #f)
(equal? (palindrome? 12321) #t)
(equal? (palindrome? 1221) #t)