#lang racket

(define (does-nth-line-start-with-n-zeroes xs line)
  (andmap zero? (take xs line))
  )

(define (triangular? mat)
  (define (helper left-over line)
    (cond
      [(null? left-over) #t]
      [(does-nth-line-start-with-n-zeroes (car left-over) line) (helper (cdr left-over) (add1 line))]
      [else #f]
      )
    )
  (helper mat 0)
  )

(triangular? '((1 2 3) (0 5 6) (0 0 9)))
(triangular? '((0 2 3) (0 0 6) (1 0 0)))
(triangular? '((1 2 3) (1 5 6) (0 0 9)))
(triangular? '((1 2 3 4) (0 5 6 7) (0 0 8 9) (0 0 0 9))) 