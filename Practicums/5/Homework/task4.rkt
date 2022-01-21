#lang racket

(define (rev-fold xs)
  (if (null? xs)
      0 ;не знам колко е кореткно, но ми се струва смислено, ако списъкът е празен, да се връща 0
      (foldr (λ (x acc) (+ (* acc 10) x)) 0  xs)
      )
  )

(define (rev-lin-rec xs)
    (if (null? xs)
        0
        (+ (car xs) (* (rev-lin-rec (cdr xs)) 10))
      )
  )

; using folding
(= (rev-fold '(1 2 3)) 321)
(= (rev-fold '(1 2 3 4 5 6 7 8 9)) 987654321)

; using a linearly recursive procedure
(= (rev-lin-rec '(1 2 3)) 321)
(= (rev-lin-rec '(1 2 3 4 5 6 7 8 9)) 987654321)