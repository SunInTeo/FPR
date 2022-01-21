#lang racket

(define (diagonal-iter xss)
  (define (helper left-over index result)
    (cond
      [(null? left-over) result]
      [else (helper (cdr left-over) (add1 index) (append result (list (list-ref (car left-over) index))))]
      )
    )
  (if (= (length (car xss)) (length xss))
      (helper xss 0 '())
      (error "Not an N by N matrix.")
      )
  )

(define (diagonal-rec xss)
  (define (helper left-over index)
    (if (null? left-over)
        '()
        (cons (list-ref (car left-over) index) (helper (cdr left-over) (add1 index)))
        )
    )
  (helper xss 0)
  )

(equal? (diagonal-rec '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))) '(1 6 11 16))