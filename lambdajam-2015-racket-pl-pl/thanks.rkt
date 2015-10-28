#lang racket
(require slideshow "util.rkt"
         "lang-slide.rkt")
(provide thanks)

(define specific-thanks-pict
  (para #:fill? #f "With help from Matthew Flatt, Eli Barzilay,"
        "Matthias Felleisen, Jay McCarthy, and all of PLT."))

(define thanks-pict 
  (vc-append 60
             (scale/improve-new-text (t "Thanks") 2.5)
             specific-thanks-pict))

(define thanks-border 100)

(define chapter-bar 
  (blackened-background client-w  (+ thanks-border (pict-height thanks-pict))))

(define (thanks)
  (with-full-leaves
   (λ ()
     (slide 
      (cc-superimpose 
       tree-color
       chapter-bar
       (colorize thanks-pict "white"))))))


(module+ main (thanks))
