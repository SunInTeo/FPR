#lang racket

(define (tabulate f)
  (λ (a b)
    (map (λ (x) (cons x (f x))) (range a (add1 b)))
    )
  )

(define (tabulate-fold f)
  (λ (x y) (foldr (λ (z acc) (cons (cons z (f z)) acc)) null (range x (add1 y)))))

(equal? ((tabulate (λ (x) (* x x))) 1 5) '((1 . 1) (2 . 4) (3 . 9) (4 . 16) (5 . 25)))