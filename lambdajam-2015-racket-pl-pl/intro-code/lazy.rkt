#lang lazy
(provide get-fibs)

(define fibs
  (cons
   0
   (cons
    1
    (map + fibs (cdr fibs)))))

(define (get-fibs n)
  (!! (take n fibs)))
