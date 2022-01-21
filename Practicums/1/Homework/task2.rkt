#lang racket

(define (leap-year-one-line? n)
  (and (zero? (modulo n 4))
       (or (zero? (modulo n 400))
           (not (zero? (modulo n 100))))))

(define (is-leap-year-guards? n)
  (cond
    [(not (= (remainder n 4) 0)) #f]		
    [(not (= (remainder n 100) 0)) #t]	
    [(not (= (remainder n 400) 0)) #f]	
    [else #t]
    )
  )								

(equal? (leap-year-one-line? 2020) #t)
(equal? (leap-year-one-line? 1988) #t)
(equal? (leap-year-one-line? 1600) #t)
(equal? (leap-year-one-line? 2400) #t)
(equal? (leap-year-one-line? 2023) #f)
(equal? (leap-year-one-line? 1700) #f)
(equal? (leap-year-one-line? 1800) #f)
(equal? (leap-year-one-line? 2100) #f)

(equal? (is-leap-year-guards? 2020) #t)
(equal? (is-leap-year-guards? 1988) #t)
(equal? (is-leap-year-guards? 1600) #t)
(equal? (is-leap-year-guards? 2400) #t)
(equal? (is-leap-year-guards? 2023) #f)
(equal? (is-leap-year-guards? 1700) #f)
(equal? (is-leap-year-guards? 1800) #f)
(equal? (is-leap-year-guards? 2100) #f)