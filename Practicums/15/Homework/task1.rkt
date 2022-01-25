#lang racket
(define (validate x)
  (define (helper result curr)
  (cond
   [(= curr 0)  (if (=(remainder (apply + (greater-than-9? (reverse result))) 10) 0)
                    #T
                    #F)]
   [(> curr 9) (helper (append result (list (remainder curr 10) (* (remainder (quotient curr 10) 10) 2))) (quotient curr 100))]
   [(< curr 10) (helper (append result (list (remainder curr 10))) (quotient curr 10))]))
    (helper '() x))



(define (greater-than-9? xs)
  (define (helper result curr)
  (cond
   [(empty? curr) result]
   [(> (car curr) 9) (helper (append result (list(- (car curr) 9))) (cdr curr))]
   [(< (car curr) 10) (helper (append result (list(car curr))) (cdr curr))]
    ))
  (helper '() xs))
  
     
(equal? (validate 1714) #f)
(equal? (validate 12345) #f)
(equal? (validate 891) #f)
(equal? (validate 123) #f)
(equal? (validate 2121) #t)
(equal? (validate 4736778291034) #t)
(equal? (validate 4485756008412) #t)
(equal? (validate 4214154976719) #t)
