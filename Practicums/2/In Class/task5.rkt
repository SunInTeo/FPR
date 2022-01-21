#lang racket

(require math/number-theory)

(define (sum-divs n)
  (define (helper current-sum current-n)
    (cond
      [(= current-n 1) (+ current-sum 1)]
      [(divides? current-n n) (helper (+ current-sum current-n) (sub1 current-n))]
      [else (helper current-sum (sub1 current-n))]
       )
    )
  (cond
    [(< n 1) 0]
    [else (helper 0 n)]
    )
  )

(= (sum-divs 0) 0)
(= (sum-divs 1) 1)
(= (sum-divs 6) 12) ; 1 + 2 + 3 + 6
(= (sum-divs 12345) 19776)