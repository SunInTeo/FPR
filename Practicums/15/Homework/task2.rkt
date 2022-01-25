#lang racket

(define (sum-expr f xs)
  (λ (x)
    (define (helper left-over i)
      (if (null? left-over)
          0
          (+
           (* (car left-over) (f (expt x i)))
           (helper (cdr left-over) (+ i 1))
           )
          )
      )
    (helper xs 1)
    )
  )

(equal? ((sum-expr (λ (x) (+ 2 x)) '(0 1 2 3)) 2) 80)
(equal? ((sum-expr (λ (x) (* x 0.8)) '(0 1 2 3 4 5)) 10) 4345680.0)