#lang racket

(define (find-single-sum a b n)
  (define (helper current-n sum)
    (cond
      [(> current-n n) sum]
      [else (helper (add1 current-n) (+ sum (* (expt 2 (sub1 current-n)) b)))]
      )
    )
  (helper 1 a)
  )

(define (find-sum a b n)
  (cond
    [(< n 3) (error "n must be bigger than 3")]
    [else (+ (find-single-sum a b n) (find-single-sum a b (sub1 n)) (find-single-sum a b (- n 2)))]
    )
  )

(= (find-sum 0 2 10) 3578) ; 510 + 1022 + 2046
(= (find-sum 5 3 5) 174) ; 26 + 50 + 98