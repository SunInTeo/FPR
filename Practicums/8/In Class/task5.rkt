#lang racket

(define (naive-levenshtein a b)
  (define (helper i j)
    (if (zero? (min i j))
        (max i j)
        (min
         (add1 (helper (sub1 i) j))
         (add1 (helper i (sub1 j)))
         (+ (helper (sub1 i) (sub1 j)) (if (equal? (list-ref a (sub1 i)) (list-ref b (sub1 j)))
                                           0
                                           1
                                           ))
         )
        )
    )
  (helper (length a) (length b))
  )

(define (find-closest-words xss)
  (let
    ([sorted-list (sort
                   (map
                    (λ (xs) (cons xs (naive-levenshtein (car xs) (cadr xs))))
                    (combinations xss 2)
                    )
                   (λ (x y) (< (cdr x) (cdr y)))
                   )])
    (map car (filter (λ (x) (= (cdr x) (cdr (car sorted-list)))) sorted-list))
    )
  )

(equal? (find-closest-words '((c a t) (k i t t e n) (w r i t t e n) (h a t) (b a t))) '(((c a t) (h a t)) ((c a t) (b a t)) ((h a t) (b a t))))