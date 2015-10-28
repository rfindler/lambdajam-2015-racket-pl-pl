#lang racket/gui
(require slideshow
         slideshow/play
         "util.ss"
         "title-lib.rkt")

(provide title)

(define (my-plt-red-color dc)
  (send dc set-alpha .3)
  (send dc set-pen "red" 4 'transparent)
  (send dc set-brush "red" 'solid))
(define (my-plt-blue-color dc)
  (send dc set-alpha .3)
  (send dc set-pen "blue" 4 'transparent)
  (send dc set-brush "blue" 'solid))
(define (my-plt-lambda-color dc)
  (send dc set-alpha .6)
  (send dc set-smoothing 'smoothed)
  (send dc set-pen "white" 4 'solid)
  (send dc set-brush "white" 'solid))

(define tan-plt-title-background
  (make-plt-title-background my-plt-red-color my-plt-blue-color #f my-plt-lambda-color
                             plt-pen-color plt-pen-style
                             #:clip? #f
                             #:edge-cleanup-pen
                             (send the-pen-list find-or-create-pen "brown" 2 'solid)))

(define (strike-thru p)
 (dc
  (let ([h (pict-height p)]
        [w (pict-width p)])
    (λ (dc dx dy)
      (send dc draw-line
            dx
            (+ dy (/ h 2))
            (+ dx w)
            (+ dy (/ h 2)))))
  (pict-width p)
  (pict-height p)
  (pict-ascent p)
  (pict-descent p)))

(define (overlay-red-fat-strikethru p)
 (cc-superimpose
  p
  (linewidth 3 (colorize (strike-thru p) "red"))))

(define (fade/center-size n1 n2 p1 p2 p3)
  (let ([sizer (cc-superimpose (ghost p1) (ghost p2) (ghost p3))])
    (fade-pict n1
               (lc-superimpose p1 sizer)
               (fade-pict n2
                          (lc-superimpose p2 sizer)
                          (lc-superimpose p3 sizer)))))

(define (add-pale-background p)
  (refocus
   (cc-superimpose
    (blackened-background (+ (pict-width p) 40)
                          (+ (pict-height p) 20))
    (colorize p "white"))
   p))

(define title-and-author
  (let ()
    (define subtitle
      (scale/improve-new-text
       (htl-append (t "a ")
                   (vl-append (t "programming-language")
                              (t "programming language")))
       1.2))
    (define title (scale/improve-new-text (bt "Racket:") 4))
    ;(define title (inset raw-title 0 (- (pict-descent raw-title))))
    (define name-scale 1.8)
    
    (define name
      (scale/improve-new-text
       (t "Robby Findler")
       name-scale))
    (define aff
      (scale/improve-new-text
       (t "Northwestern & PLT")
       name-scale))
    (define title/subtitle
      (hbl-append 20 title subtitle))
    (add-pale-background
     (inset (vc-append
             10
             title/subtitle
             (hbl-append name
                         (blank
                          (- (pict-width title/subtitle)
                             (pict-width name)
                             (pict-width aff))
                          0)
                         aff))
            10 10 10 30))))

(define (do-title)
  (cc-superimpose
   tan-plt-title-background
   (vc-append
    (blank 800 0)
    title-and-author)))

(define (title) 
  (with-full-leaves
   (λ ()
     (slide (do-title)))
   #f))

(module+ main (title))
