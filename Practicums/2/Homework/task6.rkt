#lang racket

(require math/number-theory)

(define (contains-digit? n d)
  (define (helper left-over)
    (cond
      [(zero? left-over) #f]
      [(= d (remainder left-over 10)) #t]
      [else (helper (quotient left-over 10))]
      )
    )
  (helper n)
  )

(define (sum-special-primes n d)
  (define (helper current-n current-count sum)
    (cond
      [(= current-count n) sum]
      [(and (prime? current-n) (contains-digit? current-n d)) (helper (add1 current-n) (add1 current-count) (+ sum current-n))]
      [else (helper (add1 current-n) current-count sum)]
      )
    )
  (helper 2 0 0)
  )

(= (sum-special-primes 5 2) 392)
(= (sum-special-primes 5 3) 107)
(= (sum-special-primes 10 3) 462)