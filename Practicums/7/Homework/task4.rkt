#lang racket

(require math/number-theory)

(define (shuffle xs)
  (define (helper result left-over-first-half left-over-second-half)
    (if (null? left-over-first-half)
        result
        (helper (append result (list (car left-over-first-half)) (list (car left-over-second-half))) (cdr left-over-first-half) (cdr left-over-second-half))
        )
    )
  (helper '() (take xs (quotient (length xs) 2)) (drop xs (quotient (length xs) 2)))
  )

(shuffle '(2 5 1 3 4 7))
(shuffle '(1 2 3 4 4 3 2 1))
(shuffle '(1 1 2 2))