#lang racket

(require racket/trace)

(define (count-occurrences-of-digit-in-num digit num)
  (define (helper result left-over)
    (cond
      [(and (< left-over 10) (not (= left-over digit))) result]
      [(= left-over digit) (add1 result)]
      [(= (remainder left-over 10) digit) (helper (add1 result) (quotient left-over 10))]
      [else (helper result (quotient left-over 10))]
      )
    )
  (helper 0 num)
  )

(define (has-already-been-counted digit xs)
  (define (helper result left-over)
    (cond
      [(null? left-over) result]
      [(equal? digit (caar left-over)) #t]
      [else (helper result (cdr left-over))])
    )
  (helper #f xs)
  )

(define (sort-sequence xs)
   (sort xs
        #:key car <)
  )

(define (get-distribution n)
  (define (helper result current-n current-digit)
    (cond
      [(< current-n 10) (sort-sequence result)]
      [(not (has-already-been-counted (remainder current-n 10) result))
       (helper
           (append result (list (cons current-digit (count-occurrences-of-digit-in-num current-digit (* n n)))))
           (quotient current-n 10)
           (remainder (quotient current-n 10) 10))]
      [else (helper result (quotient current-n 10) (remainder (quotient current-n 10) 10))]
      )
    )
  (trace helper)
  (helper '() (* n n) (remainder (* n n) 10))
  )

(get-distribution 123)

