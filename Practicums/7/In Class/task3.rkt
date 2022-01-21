#lang racket

(require math/number-theory)

(define (sum-digit-divisors n)
  (define (helper current)
    (cond
      [(zero? current) 0]
      [(divides? (remainder current 10) n) (+ (remainder current 10) (helper (quotient current 10)))]
      [else (helper (quotient current 10))]
      )
    )
  (if (> n 0)
      (helper n)
      (error "n should be natural")
      )
  )

(define (push-back x xs)
  (append xs (list x))
  )

(define (get-partial-pairs y xs)
  (define (helper result left-over)
    (cond
      [(null? left-over) result]
      [(= y (car left-over)) (helper result (cdr left-over))]
      [else (helper (push-back (cons y (car left-over)) result) (cdr left-over))]
      )
    )
  (helper '() xs)
  )

(define (get-cartesian-product xs)
  (define (helper result left-over)
    (if (null? left-over)
        result
        (helper (append result (get-partial-pairs (car left-over) xs)) (cdr left-over))
        )
    )
  (helper '() xs)
  )

(define (same-sum a b)
  (define (helper left-over counter)
    (cond
      [(null? left-over) (/ counter 2)]
      [(= (sum-digit-divisors (caar left-over)) (sum-digit-divisors (cdar left-over))) (helper (cdr left-over) (add1 counter))]
      [else (helper (cdr left-over) counter)]
      )
    )
  (helper (get-cartesian-product (range a (add1 b))) 0)
  )

(= (same-sum 28 35) 2) ; the pairs are (28,32) and (29,34)