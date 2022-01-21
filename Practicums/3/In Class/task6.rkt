#lang racket

(define (sum-digits n)
  (if (< n 10)
      n
      (+ (remainder n 10) (sum-digits (quotient n 10)))
      )
  )

(define (digital-root n)
  (cond
    [(< n 1) (error "n has to be natural")]
    [(< n 10) n]
    [else (digital-root (sum-digits n))]
    )
  )

(= (digital-root 16) 7)
; => 1 + 6
; => 7
(= (digital-root 942) 6)
; => 9 + 4 + 2
; => 15
; => 1 + 5
; => 6
(= (digital-root 132189) 6)
(= (digital-root 493193) 2)