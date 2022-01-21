#lang racket

;from task 1
(define (rev n)
  (define (helper result left-over)
    (if (zero? left-over)
        result
        (helper  (+ (* result 10) (remainder left-over 10)) (quotient left-over 10))
    ))
  (helper 0 n)
  )
  
(define (palindrome? n)
  (= n (rev n))
  )

(define (num-palindromes-rec a b)
  (cond
    [(> a b) (num-palindromes-rec b a)]
    [else (cond
            [(= a b) (if (palindrome? a)
                         1
                         0
                         )]
            [(palindrome? a) (add1 (num-palindromes-rec (add1 a) b))]
            [else (num-palindromes-rec (add1 a) b)]
            )]
    )
  )

(define (num-palindromes-iter a b)
  (define (helper num-of-palindromes current-num upper-limit)
    (cond
      [(> current-num upper-limit) num-of-palindromes]
      [(palindrome? current-num) (helper (add1 num-of-palindromes) (add1 current-num) upper-limit)]
      [else (helper num-of-palindromes (add1 current-num) upper-limit)]
      )
    )
  (cond
    [(> a b) (helper 0 b a)]
    [else (helper 0 a b)]
    )
  )


(= (num-palindromes-rec 1 101) 19)
(= (num-palindromes-rec 1 100) 18)
(= (num-palindromes-rec 100 1) 18)

(= (num-palindromes-iter 1 101) 19)
(= (num-palindromes-iter 1 100) 18)
(= (num-palindromes-iter 100 1) 18)