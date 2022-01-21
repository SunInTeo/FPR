#lang racket

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