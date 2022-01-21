#lang racket

(define (count-of-digits n)
  (define (helper counter left-over)
    (cond
      [(zero? left-over) counter]
      [else (helper (add1 counter) (quotient left-over 10))]
      )
    )
  (helper 0 n)
)

(define (get-last-n-digits number n)
  (remainder number (expt 10 n))
  )

(define (automorphic? n)
  (cond
    [(> n 0) (= n (get-last-n-digits (* n n) (count-of-digits n)))]
    [else (error "n was not natural")]
    )
)

(equal? (automorphic? 3)#f)
(equal? (automorphic? 10)#f)
(equal? (automorphic? 5)#t)
(equal? (automorphic? 25)#t)
(equal? (automorphic? 76)#t) 
(equal? (automorphic? 890625)#t) 
(equal? (automorphic? 625)#t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)
; (automorphic? -1) ; error: n was not natural
; (automorphic? 0) ; error: n was not natural