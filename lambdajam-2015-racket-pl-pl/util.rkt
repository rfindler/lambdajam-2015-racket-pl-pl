#lang racket/gui
(require slideshow
         slideshow/code
         racket/runtime-path)
(provide timeline-focus-color background-hilite-color
         super-t contents-of chapter itm itm* 
         chapter-pict goes-to
         with-full-leaves
         blackened-background
         current-bkg)
(define goes-to (t " ⇒ "))

(define (blackened-background w h)
  (cellophane 
   (colorize (filled-rectangle w h #:draw-border? #f)
             "black")
   .7))

(define-runtime-path background "sscc.jpg")
(define bkg
  (inset (freeze
          (let* ([bmp (bitmap background)]
                 [w -150]
                 [p (clip (inset bmp w 0 0 (* w 768/1024)))])
            (scale-to-fit p 1024 768)))
         (- margin)))
(define current-bkg (make-parameter bkg))

(define nw-plt-logo (scale (bitmap "logo/nw-plt.png") 1/3))

(define original-slide-assembler (current-slide-assembler))

(current-slide-assembler
 (λ (a b c)
   (define main 
     (ct-superimpose (colorize (lb-superimpose (filled-rectangle client-w client-h)
                                               nw-plt-logo)
                               ;"antiquewhite"
                               ;"LightSteelBlue"
                               ;"Lavender"
                               "Ivory"
                               )
                     (original-slide-assembler a b c)))
     (refocus (cc-superimpose (current-bkg) main)
              main)))

(define timeline-focus-color "firebrick")
(define background-hilite-color "tan")

(current-para-width 650)
(current-title-color "Brown")

(define (super-t str)
  (parameterize ([current-main-font (cons 'superscript (current-main-font))])
    (t str)))

(define-runtime-path here ".")
(define (contents-of filename [skip-first-line? #f] #:build-line [build-line tt])
  (call-with-input-file (if (absolute-path? filename) filename (build-path here filename))
    (λ (port)
      (when skip-first-line? (read-line port))
      (let loop ()
        (let ([l (read-line port)])
          (cond
            [(eof-object? l) (blank)]
            [else (vl-append (build-line l)
                             (loop))]))))))

(define (chapter-pict str [bar-func filled-rectangle])
  (let ([words (scale (bt str) 2)])
    (cc-superimpose
     (colorize (bar-func
                client-w
                (+ (pict-height words) 40))
               background-hilite-color)
     (colorize words "white"))))

(define chapter-bar (blackened-background 950 100))

(define (chapter str) 
  (with-full-leaves
   (λ ()
     (slide 
      (cc-superimpose 
       chapter-bar
       (chapter-pict str blank))))))

(define (clip-to-blank-screen p)
  (define screen (blank 1024 768))
  (refocus (clip (refocus (cc-superimpose p screen) screen))
           p))

(define (with-full-leaves t [plt-nu-logo? #t])
  (parameterize ([current-slide-assembler
                  (λ (a b c)
                    (define bkg (current-bkg))
                    (define main (original-slide-assembler a b c))
                    (clip-to-blank-screen
                     (refocus (cc-superimpose 
                               (if plt-nu-logo?
                                   (pin-over bkg
                                             (/ (- (pict-width bkg) client-w) 2)
                                             (+ (/ (- (pict-height bkg) client-h) 2)
                                                (- client-h (pict-width nw-plt-logo)))
                                             nw-plt-logo)
                                   bkg)
                               main)
                              main)))])
    (t)))

(define bul (let* ([dingbat "✤"]
                   [m (t dingbat)])
              (hbl-append
               (refocus
                (cc-superimpose (colorize (scale/improve-new-text (t dingbat) 1.5)
                                          background-hilite-color) 
                                m)
                m)
               (t " "))))
(define (itm . x) (apply item #:bullet bul x))
(define (itm* . x) (apply item #:fill? #f #:bullet bul x))

(current-keyword-list
 (list* "enum-case"
        "define/public"
        "define-enum"
        "syntax-parse"
        (current-keyword-list)))

