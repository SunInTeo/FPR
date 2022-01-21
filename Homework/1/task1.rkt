#lang racket

(define (count-digit-occurrences num d)
  (define (helper result left-over)
    (cond
      [(zero? left-over) result]
      [(= (remainder left-over 10) d) (helper (add1 result) (quotient left-over 10))]
      [else (helper result (quotient left-over 10))]
      )
    )
  (helper 0 num)
  )

(define (sum-of-digits num)
  (if (< num 10)
      num
      (+ (remainder num 10) (sum-of-digits (quotient num 10)))
      )
  )

(define (sum-counts-iter x d)
  (define (helper result left-over)
    (cond
      [(zero? left-over) (sum-of-digits result)]
      [(helper (+ result (count-digit-occurrences left-over d)) (sub1 left-over))]
      )
    )
  (if (<= x 0)
      (error "x should be natural")
      (helper 0 x)
      )
  )

(sum-counts-iter 1 1) ; -> 1
(sum-counts-iter 5123 1) ; -> 19
(sum-counts-iter 1234 8) ; -> 10
(sum-counts-iter 5555 5) ; -> 10
(sum-counts-iter 65432 6) ; -> 11
(sum-counts-iter 70000 1) ; -> 11
(sum-counts-iter 123321 1) ; -> 29

