#lang racket

(define (get-route xs x)
  (define (helper result left-over comparator)
    (cond
      [(null? left-over) (append result (list comparator))]
      [(assoc comparator left-over) (helper
                                     (append result (list comparator))
                                     (remove (assoc comparator left-over) left-over)
                                     (cdr (assoc comparator left-over))
                                     )]
      [else (error "No such itinerary!")]
     )
    )
  (helper '() xs x)
  )

(define (itinerary xss)
  (λ (x) (get-route xss x))
    )
  
((itinerary '(("SFO" . "HKO") ("YYZ" . "SFO") ("YUL" . "YYZ") ("HKO" . "ORD"))) "YUL")
((itinerary '(("A" . "B") ("A" . "C") ("B" . "C") ("C" . "A"))) "A")
;((itinerary '(("SFO" . "COM") ("COM" . "YYZ"))) "COM")


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
