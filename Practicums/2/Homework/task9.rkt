#lang racket

(require math/number-theory)

(define (max-multiple d b)
  (define (helper current-num)
    (cond
      [(<= current-num 0) (error "Such number does not exist.")]
      [(and (divides? d current-num) (<= current-num b)) current-num]
      [else (helper (sub1 current-num))]
      )
    )
  (helper b)
  )

(= (max-multiple 2 7) 6)
(= (max-multiple 3 10) 9)
(= (max-multiple 7 17) 14)
(= (max-multiple 10 50) 50)
(= (max-multiple 37 200) 185)
(= (max-multiple 7 100) 98)