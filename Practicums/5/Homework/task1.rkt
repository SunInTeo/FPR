#lang racket

(define (remove-all-no-proc x xs) 
    (cond
      [(null? xs) '()]
      [(pair? (car xs)) (cons (remove-all-no-proc x (car xs)) (remove-all-no-proc x (cdr xs)))]
      [(equal? (car xs) x) (remove-all-no-proc x (cdr xs))]
      [else (cons (car xs) (remove-all-no-proc x (cdr xs)))]
      )
  )

(define (remove-all-proc x xs)
  (remq* (list x) xs)
  )

; without using a predefined procedure
(equal? (remove-all-no-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-no-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-no-proc 1 '(1)) '())
(equal? (remove-all-no-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-no-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))

; using a predefined procedure
(equal? (remove-all-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-proc 1 '(1)) '())
(equal? (remove-all-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))