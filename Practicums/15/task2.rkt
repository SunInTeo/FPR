#lang racket

#|If g is myPoly [2.7, 3.0 ..]
    then g 2.2 3 -> -0.4399999999999998
If g is myPoly [2, 3 ..]
    then g 2.2 3 -> 0.2880000000000002|#

(define (my-poly xs)
  (λ (x y) (foldr (λ (n acc) (* acc (- x n))) 1 (take xs y)))
  )

((my-poly (range 2.7 101 0.3)) 2.2 3)
((my-poly (range 2 101)) 2.2 3)