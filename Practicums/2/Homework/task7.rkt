#lang racket

(define (count-occurences num digit)
  (define (helper counter left-over)
    (cond
      [(zero? left-over) counter]
      [(= (remainder left-over 10) digit) (helper (add1 counter) (quotient left-over 10))]
      [else (helper counter (quotient left-over 10))]
      )
    )
  (cond
    [(> num 0) (helper 0 num)]
    [else (error "Negative number!")]
    )
  )

(= (count-occurences 121 1) 2)
;(count-occurences -121 1) ; error "Negative number!"