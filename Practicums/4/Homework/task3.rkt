#lang racket

(define (apply-n f n)
  (λ (x)
    (cond
      [(<= n 0) (error "n cannot be negative")] ;отново стои същият въпрос, просто не мисля, че е излишно
      [(= n 1) (f x)]
      [else ((apply-n f (sub1 n)) (f x))]
      )
    )
  )

(= ((apply-n (λ (x) (* 2 x)) 5) 2) 64)
(= ((apply-n (λ (x) (quotient x 10)) 2) 100) 1)