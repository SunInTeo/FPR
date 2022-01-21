#lang racket

(define (filter-and-sort-list xs)
    (sort (remove-duplicates (filter negative? xs)) >)
    )

(define (kth-max-min xs)
  (λ (k) (if (>= (length (filter-and-sort-list xs)) k)
             (list-ref (filter-and-sort-list xs) (sub1 k))
             (error "No such number!")
             ))
  )

(= ((kth-max-min '(-1)) 1) -1)
(= ((kth-max-min '(-1 -5 -6 -6 -6 -6)) 2) -5)
(= ((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2) -2)
;((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3) ; error: No such number!