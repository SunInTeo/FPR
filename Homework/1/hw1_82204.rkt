#lang racket

; task1

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


; task2

(define (count-digits n)                       
  (if (< n 10)                     
      1                                       
      (+ 1 (count-digits (quotient n 10)))
      )
  ) 

(define (add-ones n)
  (define (helper result current-result left-over carry)
    (cond
      [(zero? left-over) (quotient result 10)]
      [(= carry 1) (helper (+ result (* (+ current-result 1) (expt 10 (count-digits result))))
                           (remainder (quotient left-over 10) 10)
                           (quotient left-over 10)
                           (if (< (+ (remainder left-over 10) 1) 10)
                               0
                               1
                               )
                           )]
      [else (helper (+ result (* (+ current-result 1) (expt 10 (count-digits result))))
                    (remainder (quotient left-over 10) 10)
                    (quotient left-over 10)
                    (if (< (+ (remainder left-over 10) 1) 10)
                               0
                               1
                               )
                    )]
      )
    )
  (cond
    [(<= n 0) (error "n should be positive")]
    [else (helper 0 (remainder n 10) n 0)]
    )
  )

(add-ones 123) ; -> 234
(add-ones 193) ; -> 2104
(add-ones 998) ; -> 10109
(add-ones 9999) ; -> 10101010