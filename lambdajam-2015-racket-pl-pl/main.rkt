#lang racket
(require slideshow
         "title.rkt"
         "or.rkt"
         "salad-bar.rkt"
         "hdl-ex.rkt"
         "util.rkt"
         "lang-slide.rkt"
         "open-compiler.rkt"
         "enum.rkt"
         "thanks.rkt"
         racket/runtime-path)

(title)

(define-runtime-path intro-code "intro-code")
(when condense?
  (define (fetch-code filename)
    (scale
     (vl-append
      20
      (t (format "~a:" filename))
      (hc-append
       (blank 20 0)
       (apply
        vl-append
        (call-with-input-file (build-path intro-code filename)
          (λ (port)
            (for/list ([l (in-lines port)])
              (tt l)))))))
     1))
  (slide (vl-append
          (t "The next three slides are the final")
          (t "version of the code shown in the demo")))
  (define one (fetch-code "fib.scrbl"))
  (define two (fetch-code "fibs.rkt"))
  (define three (fetch-code "lazy.rkt"))
  (define (show a b c)
    (slide
     (ltl-superimpose
      (a one)
      (b two)
      (c three))))
  (show values ghost ghost)
  (show ghost values ghost)
  (show ghost ghost values))

(open-compiler)

(macros-are-not-salad-bars)

(slide
 (vl-append
  20
  (para #:fill? #f "Outline:")
  (hc-append
   (blank 40 0)
   (vl-append
    20
    (itm* "A macro challenge")
    (itm* "Scope-aware macro systems")
    (itm* "mini-hdl")))))

(or-explanation)

(chapter "Scope, scope, scope")

(enum-slides)

(chapter "mini-hdl")

(hdl-ex)

(lang-slide)

(chapter "Conclusions")

(slide
 (itm "Macros matter")
 (blank) (blank)
 (itm "Need a new language? Try Racket"))

(thanks)
