#lang racket

(require math/number-theory)


(define (num-prime? n)
  (define (helper current-num)
    (cond
      [(>= current-num n) #t]
      [(divides? current-num n) #f]
      [else (helper (add1 current-num))]
      )
    )
  (cond
    [(<= n 0) (error "n was negative")]
    [(= n 1) #f]
    [else (helper 2)]
    )
  )

(equal? (num-prime? 1) #f)
(equal? (num-prime? 2) #t)
(equal? (num-prime? 3) #t)
(equal? (num-prime? 6) #f)
(equal? (num-prime? 61) #t)