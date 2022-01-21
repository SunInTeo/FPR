#lang racket

(define (remove-first-occurrence num digit)
  (define (helper is-found found-digit left-over counter)
    (cond
      [(zero? left-over)
       (cond
         [is-found found-digit]
         [else (error "There was no such digit found.")]
         )]
      [(and (not is-found) (= digit (remainder left-over 10))) (helper #t found-digit (quotient left-over 10) counter)]
      [else (helper is-found (+ (* (remainder left-over 10) (expt 10 counter)) found-digit) (quotient left-over 10) (add1 counter))]
      )
    )
  (helper #f 0 num 0)
  )

(= (remove-first-occurrence 15365 5) 1536)
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 1212 1) 122)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence 1212 1) 1) 22)