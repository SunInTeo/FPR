#lang racket

(require racket/trace)

(define (int->list num)
  (define (helper result current-num)
    (if (< current-num 1)
        (reverse result)
        (helper (append result (list (remainder current-num 10))) (quotient current-num 10))
        )
    )
  (trace helper)
  (helper '() num)
  )

(define (product-of-digits list)
  (apply * list)
  )

(define (persistence n)
  (define (helper result current-n)
    (cond
      [(< current-n 10) (cons result (length result))]
      [else (helper (append result (list (product-of-digits (int->list current-n)))) (product-of-digits (int->list current-n)))]
      )
    )
  (helper '() n)
  )

(persistence 39)
(persistence 126)
(persistence 4)
(persistence 999)
