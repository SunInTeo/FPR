#lang racket

(define (find-max n)
  (define (helper current-n max-n)
    (cond
      [(zero? current-n) max-n]
      [(> (remainder current-n 10) max-n) (helper (quotient current-n 10) (remainder current-n 10))]
      [else (helper (quotient current-n 10) max-n)]
      )
    )
  (helper (quotient n 10) (remainder n 10))
  )

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

(define (count-of-digits-rec n)
  (cond
    [(< n 0) (error "n should be positive")]
    [(< n 10) 1]
    [else (add1 (count-of-digits-rec (quotient n 10)))]
    )
  )

(define (sort-n n)
  (define (helper result left-over counter counter-digits)
    (cond
      [(zero? left-over)
       (cond
         [(= counter counter-digits) result]
         [(< counter counter-digits) (* result (expt 10 (- counter-digits counter)))]
         )]
      [else (helper (+ (* result 10) (find-max left-over)) (remove-first-occurrence left-over (find-max left-over)) (add1 counter) counter-digits)]
      )
    )
  (helper 0 n 0 (count-of-digits-rec n))
)

(= (sort-n 1714) 7411)
(= (sort-n 123450) 543210)
(= (sort-n 123405) 543210)
(= (sort-n 123045) 543210)
(= (sort-n 120345) 543210)
(= (sort-n 102345) 543210)
(= (sort-n 8910) 9810)
(= (sort-n 321) 321)
(= (sort-n 29210) 92210)
(= (sort-n 1230) 3210)
(= (sort-n 55345) 55543)
(= (sort-n 14752) 75421)
(= (sort-n 329450) 954320)
(= (sort-n 9125) 9521)