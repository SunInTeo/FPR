#lang racket

(define (have-matching-lengths xss yss)
  (if (and (null? xss) (null? yss))
      #t
      (if (or (null? xss) (null? yss))
          #f
          (have-matching-lengths (cdr xss) (cdr yss))
          )
      )
  )
  

(equal? (have-matching-lengths '((1 2 3) (4 5 6) (7 8 9)) '((1 4 7) (2 5 8) (3 6 9))) #t)
(equal? (have-matching-lengths '((1 2)) '((1 4 7) (2 5 8))) #f)