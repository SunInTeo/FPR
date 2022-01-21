#lang racket

(define (zero-rows xss)
  (map (λ (xs) (if (ormap zero? xs)
                   (map (λ (x) 0) xs)
                   xs
                   )) xss)
  )

(define (zero-cols xss)
  (apply map list (zero-rows (apply map list xss)))
  )

(equal? (zero-cols '((1 2 0) (3 4 1) (0 5 7) (4 2 4))) '((0 2 0) (0 4 0) (0 5 0) (0 2 0)))