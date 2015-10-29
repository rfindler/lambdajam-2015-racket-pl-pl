#lang racket/base
(require "util.rkt"
         lang-slide
         racket/gui/base
         slideshow/code
         slideshow
         slideshow/play)

(provide lang-slide tree-color)

(define hudak-quote
  (hbl-append (t "“A domain specific language is the ultimate abstraction.” ")
              (t "— Paul Hudak")))

(define tree-bw (freeze (langs-in-tree #f)))
(define tree-color (freeze (langs-in-tree #t)))

(define (lang-slide)
  (parameterize ([current-code-font 'default])
    (define bw-placement (ghost (launder tree-bw)))
    (define color-placement (ghost (launder tree-bw)))
    (define rhs 
      (freeze
       (scale/improve-new-text
        (apply vl-append
               (langs-with-colors))
        .7)))
    (play-n
     #:title "Files in Racket" 
     #:layout 'tall
     #:steps 20
     (λ (n)
       (define-values (n1 n2) (values (fast-start n) (fast-end n)))
       (slide-pict (cc-superimpose
                    bw-placement
                    (vc-append 40 
                               (fade-pict n2 (ghost hudak-quote) hudak-quote)
                               (hc-append 40
                                          color-placement
                                          (fade-pict n2 (ghost rhs) rhs))))
                   (fade-pict n2 tree-bw tree-color)
                   bw-placement
                   color-placement
                   n1)))))

(module+ main (lang-slide))
