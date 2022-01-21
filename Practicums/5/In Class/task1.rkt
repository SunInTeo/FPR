#lang racket

(define (sort-list xs)
  (λ (proc) (sort xs proc))
  )

((sort-list '("one" "two" "0" "five" "" "one hundred" "onehundred")) (λ (x y) (< (string-length x) (string-length y))))