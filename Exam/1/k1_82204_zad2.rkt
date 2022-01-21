#lang racket

(define (get-length-sequence xss)
  (define (helper result left-over)
    (cond
      [(null? left-over) (sort result <)]
      [else (helper (append result (list (length (car left-over)))) (cdr left-over))]
      )
    )
  (helper '() xss)
  )

(define (get-missing-length xss)
  (define (helper left-over)
    (cond
      [(null? left-over) 0]
      [(> (- (cadr left-over) (car left-over)) 1) (/ (+ (cadr left-over) (car left-over)) 2)]
      [else (helper (cdr left-over))]
      )
    )
  (if (or (null? xss) (ormap null? xss))
      (error "Empty list!")
      (helper (get-length-sequence xss))
      )
  )

(get-missing-length '((1 2) (4 5 1 1) (1) (5 6 7 8 9)))
(get-missing-length '(("a", "a", "a") ("a", "a") ("a", "a", "a",
"a") ("a") ("a", "a", "a", "a", "a", "a")))

