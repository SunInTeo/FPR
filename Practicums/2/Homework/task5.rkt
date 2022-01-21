#lang racket

(require math/number-theory)

; from task 5
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

(define (amicable? a b)
  (= (sum-divs a) (sum-divs b))
  )

(equal? (amicable? 200 300) #f)
(equal? (amicable? 220 284) #t)
(equal? (amicable? 284 220) #t)
(equal? (amicable? 1184 1210) #t)
(equal? (amicable? 2620 2924) #t)
(equal? (amicable? 6232 6368) #t)