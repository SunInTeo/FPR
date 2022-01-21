#lang racket

(define (count-digits-rec n)
  (cond
    [(< n 0) (error "n was negative")]
    [(< n 10) 1]
    [else (+ 1 (count-digits-rec (quotient n 10)))]
    )
  )

(define (count-digits-iter n)
  (define (helper current-n result)
    (cond
      [(< current-n 10) (add1 result)]
      [else (helper (quotient current-n 10) (add1 result))]
      )
    )
  (cond
    [(< n 0) (error "n was negative")]
    [else (helper n 0)]
    )
  )

(= (count-digits-rec 12345) 5)
(= (count-digits-rec 123) 3)

(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)
; (count-digits-iter -13) ; error "n was negative"