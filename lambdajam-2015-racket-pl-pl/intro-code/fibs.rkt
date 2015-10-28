#lang typed/racket
(provide fibs)
(require/typed
 "lazy.rkt"
 [get-fibs
  (-> Integer (Listof Integer))])

(: fibs (-> Integer String))
(define (fibs n)
  (apply
   string-append
   (add-between
    (map
     number->string
     (get-fibs n))
    ", ")))
