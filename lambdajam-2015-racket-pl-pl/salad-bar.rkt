#lang racket
(require slideshow "util.ss")
(provide macros-are-not-salad-bars)

(define footnote
  (let ([note (scale (hbl-append (super-t "†")
                                 (t "With thanks (apologies) to Will Clinger and Jonathan Rees"))
                     1/2)])
    (pin-over
     (blank)
     (- (/ client-w 2) (pict-width note) 20)
     (- client-h (pict-height note) 100)
     note)))

(define sqr-pict #f)

(define (build-sb-line l)
  (cond
    [(regexp-match #rx"^(.*)(sqr\\([0-9+,]*\\))(.*)$" l)
     =>
     (λ (m)
       (set! sqr-pict (tt (list-ref m 2)))
       (hbl-append (tt (list-ref m 1))
                   sqr-pict
                   (tt (list-ref m 3))))]
    [else (tt l)]))

(define saladbar.c (contents-of "salad-bar.c" #t))
(define sqr.c (contents-of "sqr.c" #t #:build-line build-sb-line))
(define sqr-m.c (contents-of "sqr-m.c" #t))
(define sqr-m2.c (contents-of "sqr-m2.c" #t))

(define (highlight main sub)
  (pin-under main
             sub
             cc-find
             (let ([p (blank)])
               (refocus
                (cc-superimpose
                 p
                 (colorize (filled-rectangle (+ (pict-width sub) 8)
                                             (+ (pict-height sub) 4))
                           background-hilite-color))
                p))))

(define (below main sub thing-below)
  (pin-under main
             sub
             cb-find
             (let ([p (blank)])
               (refocus
                (ct-superimpose
                 p
                 thing-below)
                p))))

(define spacer (ghost (launder (cc-superimpose saladbar.c
                                               sqr.c
                                               sqr-m.c
                                               sqr-m2.c))))

(define name "Good macros are not salad bars")
(define title (colorize (hbl-append (t name) (super-t "†"))
                        (current-title-color)))

(define (with-below str-below cross-out)
  (below (highlight sqr.c sqr-pict)
         sqr-pict
         (vc-append (cc-superimpose (colorize goes-to "red")
                                    (cross-out (colorize (t "/") "red")))
                    str-below)))

(define (mk-below second-one third-one)
  (let ([main (tt "3+2*3+2")])
    (refocus (htl-append main
                         (vl-append (second-one (tt " = 3+(2*3)+2"))
                                    (third-one (tt " = 11"))))
             main)))

(define (macros-are-not-salad-bars)
  (stage saladbar.c)
  (stage sqr.c)
  (stage (vl-append 10 sqr.c (colorize (hbl-append goes-to (tt "11")) "red")))
  (stage (with-below (tt "(3+2)*(3+2)") values))
  (stage (with-below (mk-below ghost ghost) ghost))
  (stage (with-below (mk-below values ghost) ghost))
  (stage (with-below (mk-below values values) ghost)))

(define (stage p)
  (slide
   #:layout 'top
   #:title title
   #:name name
   footnote
   (lt-superimpose p spacer)))

;(macros-are-not-salad-bars)
