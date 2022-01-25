#lang racket

(define (rf f g)
  (λ (ns h) (map h (filter (λ (n) (> (f n) (g n))) ns))))

((rf (lambda (x) (* 2 x)) (lambda (x) (+ 2 x))) '(1 2 3 4 5) (lambda (x) (* 2 x))) ; --> '(6 8 10)
((rf (lambda (x) (* 2 x)) (lambda (x) (+ 2 x))) '(1 2 3 4 5) (lambda (x) (* 3 x))) ; --> '(9 12 15)
