#lang racket

(define (get-count-of-greater-elements x xs)
  (cond
    [(empty? xs) 0]
    [(> (car xs) x) (add1 (get-count-of-greater-elements x (cdr xs)))]
    [else (get-count-of-greater-elements x (cdr xs))]
    )
  )

(define (num-bigger-elements xs)
  (define (helper result left-over)
    (if (empty? left-over)
        result
        (helper (append result (list (cons (car left-over) (get-count-of-greater-elements (car left-over) xs)))) (cdr left-over))
        )
    )
  (helper '() xs)
  )


(equal? (num-bigger-elements '(5 6 3 4)) '((5 . 1) (6 . 0) (3 . 3) (4 . 2)))
(equal? (num-bigger-elements '(1 1 1)) '((1 . 0) (1 . 0) (1 . 0)))