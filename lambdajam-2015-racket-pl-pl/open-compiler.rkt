#lang racket
(require "util.rkt"
         slideshow
         slideshow/play
         slideshow/code
         racket/runtime-path)

(provide open-compiler)

(define (open-compiler)
  (open-compiler-animate)
  (secret-sauce)
  (macro-defn))

(define (scale-to-fit/big p w h)
  (define s (max (/ w (pict-width p))
                 (/ h (pict-height p))))
  (scale p s))

(define (inset-to-fit p w h)
  (define x-amt (- (/ (max 0 (- (pict-width p) w)) 2)))
  (define y-amt (- (/ (max 0 (- (pict-height p) h)) 2)))
  (inset p x-amt y-amt x-amt y-amt))

(define-runtime-path sauce.jpg "sauce.jpg")
(define (secret-sauce)
  (with-full-leaves
   (λ ()
     (slide
      (cc-superimpose
       (inset-to-fit (scale-to-fit/big (bitmap sauce.jpg)
                                       1024
                                       768)
                     client-w
                     client-h)
       (colorize (vc-append
                  -20
                  (scale/improve-new-text
                   (t "secret sauce:")
                   2.5)
                  (scale/improve-new-text
                   (t "Macros")
                   4))
                 "white"))))
   #f))

(define (macro-defn)
  (slide
   #:title "Macro systems"
   (para "A" (tt "macro")
         "extends a language by specifying how to compile a new feature into existing features")))


(define (open-compiler-animate)
  (define compiler (scale/improve-new-text (t "compiler") 1.5))
  (define runtime (scale/improve-new-text (t "runtime") 1.5))
  (define s (+ (max (pict-width compiler) (pict-width runtime)) 100))
  (define the-box (linewidth 8 (frame (blank s s))))
  (define runtime-place (cc-superimpose the-box runtime))
  (define small-compiler (cc-superimpose (ghost the-box) compiler))
  (define clauses
    (list (code [(if a b c)       «code»])
          (code [(+ a ...)        «code»])
          (code [(λ (x ...) e)    «code»])
          (code [(#%app a b ...)  «code»])
          (code [(and a ...)      «code»])
          (code [(or a ...)       «code»])
          (code [(cond [q a] ...) «code»])
          (code [(define x e)     «code»])))
  (define (compiler/code n)
    (inset
     (code
      (define (compile exp)
        (syntax-parse exp
          #,(apply vl-append
                   (for/list ([clause (in-list clauses)])
                     (define extra-space 14)
                     (inset clause
                            0 (* n extra-space)
                            0 (* n extra-space)))))))
     20))
  (define large-compiler-spot (ghost (launder (compiler/code 0))))
  (define main
    (cc-superimpose
     large-compiler-spot
     (hc-append
      100
      (ghost runtime-place)
      (ghost small-compiler))))
  (define-values (rx ry) (lt-find main runtime-place))

  (define (slide-and-scale main from to n)
    (define extra-space 8)
    (define to-slide
      (linewidth
       (+ (* (- 1 n) 8) (* n 6))
       (frame
        (blank (+ (* (- 1 n) (pict-width from))
                  (* n (+ extra-space (pict-width to))))
               (+ (* (- 1 n) (pict-height from))
                  (* n (+ extra-space (pict-height to))))))))
    (define-values (x y) (lt-find main to))
    (cond
      [(zero? n) main]
      [else
       (pin-over main
                 (* n (+ (* -1/2 extra-space) x))
                 (* n (+ (* -1/2 extra-space) y))
                 to-slide)]))

  (define (slide-and-scale/all main from n)
    (for/fold ([main main])
              ([clause (in-list clauses)])
      (slide-and-scale main from clause n)))
  
  (play-n
   (λ (n1 n2 n3)
     (define compiler
       (refocus
        (cc-superimpose

         (cellophane (linewidth
                      8
                      (frame
                       (blank (+ (* n1 (pict-width large-compiler-spot))
                                 (* (- 1 n1) (pict-width the-box)))
                              (+ (* n1 (pict-height large-compiler-spot))
                                 (* (- 1 n1) (pict-height the-box))))))
                     (if (zero? n3) 1 0))
         
         (cellophane small-compiler (- 1 n2))
         (cellophane (compiler/code n3) n2))
        small-compiler))
     (slide-and-scale/all
      (cc-superimpose
       (pin-over
        (slide-pict/center main
                           compiler
                           small-compiler
                           large-compiler-spot n1)
        (- rx (* n1 500)) ry
        runtime-place))
      large-compiler-spot
      n3))))
      

(module+ main
  (open-compiler))