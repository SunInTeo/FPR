#lang racket

(define (get-partial-pairs y xs)
  (define (helper result left-over)
    (cond
      [(null? left-over) result]
      [(= y (car left-over)) (helper result (cdr left-over))]
      [else (helper (append result (list (cons y (car left-over)))) (cdr left-over))]
      )
    )
  (helper '() xs)
  )
 
(define (my-cartesian-product xs ys)
  (define (helper result left-over)
    (if (null? left-over)
        result
        (helper (append result (get-partial-pairs (car left-over) ys)) (cdr left-over))
        )
    )
  (helper '() xs)
  )

(equal? (my-cartesian-product '(1 2) '(3 4)) '((1 . 3) (1 . 4) (2 . 3) (2 . 4)))
(equal? (my-cartesian-product '(1 2 3 4 5) '(6 7 8)) '((1 . 6) (1 . 7) (1 . 8) (2 . 6) (2 . 7) (2 . 8) (3 . 6) (3 . 7) (3 . 8) (4 . 6) (4 . 7) (4 . 8) (5 . 6) (5 . 7) (5 . 8)))