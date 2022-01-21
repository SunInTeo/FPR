#lang racket

(define (insert-at x idx xs)
  (cond
    [(empty? xs) (list x)]
    [(= idx 0) (cons x xs)]
    [(or (negative? idx) (> idx (length xs))) (error "Incorrect index.")]
    [else (cons (car xs) (insert-at x (sub1 idx) (cdr xs)))]
    )
  )

(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 1 '(1 2 3)) '(1 10 2 3))