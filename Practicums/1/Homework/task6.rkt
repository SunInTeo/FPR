#lang racket

(define (rev x)
  (define (helper x revX)
    (if (zero? x)
        revX
        (let ((lastDigit (remainder x 10)))
          (helper (/ (- x lastDigit) 10) (+ (* revX 10) lastDigit))))
    )
  (helper x 0)
  )

(= (rev 1) 1)
(= (rev 123) 321)
(= (rev 987654321) 123456789)