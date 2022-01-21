#lang racket

(define (assoc-rec key xs)
  (cond
    [(null? xs) (error "List was empty")]
    [(= (length xs) 1) (if (= (caar xs) key) (cdar xs) (error "Element not found"))]
    [(= (car (car xs)) key) (cdr (car xs))]
    [else (assoc-rec key (cdr xs))]
    )
  )

(define (assoc-hop key xs)
  (cdar (dropf xs (λ (x) (not (equal? key (car x))))))
  )

(define (assoc-assoc key xs)
  (cdr (assoc key xs))
  )

(define (assoc-hop-filter key xs)
  (cdar (filter (λ (x) (equal? key (car x)))))
  )

; using a recursive process
(equal? (assoc-rec 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")

; using a higher order procedure
(equal? (assoc-hop 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")

; using assoc
(equal? (assoc-assoc 1 (list '(2 . "Two") '(3 . "Three") '(1 . "One"))) "One")