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

(= (naive-levenshtein '(c a t) '(d o g)) 3)
(= (naive-levenshtein '(c a t) '(h a t)) 1)
(= (naive-levenshtein '(k i t t e n) '(w r i t t e n)) 2)