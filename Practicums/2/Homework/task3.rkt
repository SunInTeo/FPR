#lang racket

(require math/number-theory)

; used from task 4
(define (num-prime? n)
  (define (helper current-num)
    (cond
      [(>= current-num n) #t]
      [(divides? current-num n) #f]
      [else (helper (add1 current-num))]
      )
    )
  (cond
    [(< n 0) (error "n was negative")]
    [(= n 1) #f]
    [else (helper 2)]
    )
  )

(define (sum-prime-divs-rec num)
  (define (helper current-num)
    (cond
      [(< num current-num) 0]
      [(and (divides? current-num num) (num-prime? current-num)) (+ current-num (helper (add1 current-num)))]
      [else (helper (add1 current-num))]
      )
    )
  (helper 2); because 1 and 0 are not prime numbers
  )

(= (sum-prime-divs-rec 0) 0)
(= (sum-prime-divs-rec 6) 5) ; 2 + 3
(= (sum-prime-divs-rec 18) 5) ; 2 + 3
(= (sum-prime-divs-rec 19) 19)
(= (sum-prime-divs-rec 45136) 53)