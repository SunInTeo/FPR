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
