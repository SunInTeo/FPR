#lang racket

(define (pad xs)
  (λ (x) (append
          (list (make-list (+ 2 (length (car xs))) x))
          (for*/list ([i xs]
                      [j (list x)])
            (append (list j) i (list j))) 
          (list (make-list (+ 2 (length (car xs))) x))
          )
    )
  )

((pad '( (1 2 3) (4 5 6) (7 8 9) )) 0)
((pad '( (1 2 3) (4 5 6) (7 8 9) )) 9)
