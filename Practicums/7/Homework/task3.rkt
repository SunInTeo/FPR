#lang racket

(require math/number-theory)

(define (int->list n)
  (if (zero? n)
      '()
      (append (int->list (quotient n 10)) (list (remainder n 10)))
      )
  )

(define (sum-of-sequence list-digits p)
  (define (helper result left-over current-p)
    (if (null? left-over)
        result
        (helper (+ result (expt (car left-over) current-p)) (cdr left-over) (add1 current-p))
        )
    )
  (helper 0 list-digits p)
  )

(define (dig-pow n p)
  (if (divides? n (sum-of-sequence (int->list n) p))
      (quotient (sum-of-sequence (int->list n) p) n)
      -1
      )
  )

(dig-pow 89 1)
(dig-pow 92 1)
(dig-pow 695 2)
(dig-pow 46288 3)