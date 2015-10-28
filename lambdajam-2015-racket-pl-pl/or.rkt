#lang racket
(require "util.rkt"
         slideshow
         slideshow/balloon
         slideshow/code
         slideshow/play
         (for-syntax racket syntax/parse))
(provide or-explanation)

(define (or-stages . args)
  (define list-args (filter (compose not procedure?) args))
  (define top-spacer (launder (ghost (apply cc-superimpose (map cadr list-args)))))
  (define side-spacer (launder
                       (ghost
                        (apply
                         cc-superimpose
                         (map (λ (x) (if (null? (cddr x))
                                         (blank)
                                         (apply cc-superimpose (cddr x))))
                              list-args)))))
  (for/fold ([previous-slide #f])
            ([x (in-list args)]
             [i (in-naturals)])
    (define first? (zero? i))
    (cond
      [(procedure? x) (x top-spacer side-spacer previous-slide)]
      [else
       (define title (car x))
       (define top (cadr x))
       (define transforms (cddr x))
       (define slide-picts
         (for/list ([transform (in-list transforms)])
           (vl-append
            ((if first? ct-superimpose lt-superimpose) top top-spacer)
            (blank 0 40)
            (lt-superimpose side-spacer transform))))
       (for ([slide-pict (in-list slide-picts)])
         (slide #:title title slide-pict))
       (last slide-picts)]))

  (void))

(define original-program
  (code 
   (define (01-list? x)
     (or (null? x)
         (null? (cdr x))))))

(define 012-program
  (code
   (define (012-list? x)
     (or (or (null? x)
             (null? 
              (cdr x)))
         (null? (cddr x))))))

(define (err-msg s) (colorize (it s) "red"))

(define var-capture-program
  (code
   (define (01-list? x)
     (let ([x (null? x)])
       (if x
           x
           (null? (cdr x)))))))


(define (x-sup sup color)
  (let ([ex (tt "x")]
        [sup (parameterize ([current-main-font (list* 'superscript 'bold 'modern)])
               (t sup))])
    (hbl-append (refocus 
                 (colorize (hbl-append ex sup)
                           color)
                 ex)
                (blank (pict-width sup)
                       0))))

(define x-0 (x-sup "0" "peru"))
(define x-1 (x-sup "1" "firebrick"))

(define (rewrites a b) (ht-append a goes-to b))

(define challenge
  (cc-superimpose (blank client-w 0)
                  
                  (vl-append
                   2
                   (para #:width 400
                         #:fill? #f
                         "Design an" (tt "or") "operation:")
                   
                   (ht-append (blank 40 0) 
                              (code (or exp_a exp_b))
                              (blank 50 0))
                   
                   (para #:width 400
                         #:fill? #f
                         "that returns the first “true” result and is short-circuiting"))))

(define d-s/x (code define-syntax))
(define or/x (code or))
(define stx/x (code stx))
(define s-p/x (code syntax-parse))
(define x-exp-pat/x (code x-exp))
(define x-exp-res/x (code x-exp))

;; WARNING: DO NOT RE-INDENT!
(define duplicated-code-macro-template
  (code (#,d-s/x (#,or/x #,stx/x)
          (#,s-p/x stx
            [(or #,x-exp-pat/x y-exp)
             #'(if x-exp #,x-exp-res/x y-exp)]))))

(define or-macro-with-let
  (code
   (define-syntax (or stx)
     (syntax-case stx
       [(or x-exp y-exp)
        #'(let ([x x-exp])
            (if x x y-exp))]))))

(define (or-explanation)
  (or-stages
   (list "Challenge"
         challenge
         (blank))
   (list "Challenge"
         challenge
         original-program)
   (list "Non-solution 1: function"
         (code
          (define (or x y)
            (if x
                x
                y)))
         original-program
         (vl-append original-program
                    (blank 0 40)
                    (hbl-append
                     (code (01-list '()))
                     goes-to
                     (err-msg "cdr: given '()"))))
   
   (list "Non-solution 2: duplicate code"
         duplicated-code-macro-template
         original-program)
   
   macro-introduction
   
   (list "Non-solution 2: duplicate code"
         duplicated-code-macro-template       
         (rewrites
          original-program
          (code
           (define (01-list? x)
             (if (null? x)
                 (null? x)
                 (null? (cdr x))))))
         012-program
         (rewrites 012-program
                   (code
                    (define (012-list? x)
                      (if (or (null? x)
                              (null?
                               (cdr x)))
                          (or (null? x)
                              (null?
                               (cdr x)))
                          (null? (cddr x))))))
         (rewrites 012-program
                   (code
                    (define (012-list? x)
                      (if (if (null? x)
                              (null? x)
                              (null?
                               (cdr x)))
                          (if (null? x)
                              (null? x)
                              (null?
                               (cdr x)))
                          (null? (cddr x))))))
         (code
          (define (0123-list? x)
            (or (or (or (null? x)
                        (null? (cdr x)))
                    (null? (cddr x)))
                (null? (cdddr x)))))
         (rewrites (code
                    (or (test-and-set 'x)
                        (test-and-set 'y)))
                   (code
                    (if (test-and-set 'x)
                        (test-and-set 'x)
                        (test-and-set 'y)))))
   
   (list "Non-solution 3: variable capture"
         or-macro-with-let
         original-program
         (rewrites original-program var-capture-program)
         (vl-append (rewrites original-program var-capture-program)
                    (blank 0 40)
                    (hbl-append
                     (code (01-list? (list 1)))
                     goes-to
                     (err-msg "cdr: given #f"))))
   
   (let ([x-0-original-program
          (code
           (define (01-list? #,x-0)
             (or (null? #,x-0)
                 (null? (cdr #,x-0)))))])
     (list "Hygiene"
           or-macro-with-let
           original-program
           x-0-original-program
           (vl-append
            (rewrites 
             x-0-original-program
             (code
              (define (01-list? #,x-0)
                (let ([#,x-1 (null? #,x-0)])
                  (if #,x-1
                      #,x-1
                      (null? 
                       (cdr #,x-0)))))))
            (blank 0 40)
            (cc-superimpose (blank client-w 0)
                            (vl-append
                             (para "Fix the macro expander: ")
                             (itm  "Each expansion stage gets its own variables")
                             (itm  "Thus variables are safe to use in macros"))))))))

(define (macro-introduction top-spacer side-spacer previous-slide)

  (define (put-at where do-find . pip-wrap-balloon-args)
    (define boon (apply pip-wrap-balloon pip-wrap-balloon-args))
    (cc-superimpose (vl-append
                     top-spacer
                     (blank 0 40)
                     side-spacer)
                    (pin-over
                     scaled-duplicated-code-macro-template
                     where
                     do-find
                     boon)))

  (define scaled-duplicated-code-macro-template
    (vc-append (scale duplicated-code-macro-template 1.4)
               (blank 0 200)))

  (define (scale-and-move n)
     (define start (launder (ghost duplicated-code-macro-template)))
     (define end (launder (ghost scaled-duplicated-code-macro-template)))
     (slide-pict
      (lt-superimpose
       (cellophane
        (drop-vertical-up-to previous-slide (pict-height top-spacer))
        (- 1 n))
       (cc-superimpose (vl-append
                        (lt-superimpose start top-spacer)
                        (blank 0 40)
                        (lt-superimpose side-spacer (blank)))
                       end))
      (scale duplicated-code-macro-template (interp-between 1 1.4 n))
      start end n))

  (define (interp-between start end n)
    (+ start (* n (- end start))))

  (define our-first-macro "Our First Macro")
  
  (play-n
   #:skip-first? #t
   #:title our-first-macro scale-and-move)
  
  (slide #:title our-first-macro
         (put-at d-s/x
                 ct-find
                 (vl-append (t "defining something")
                            (t "for compile-time"))
                 's 0 30))
  (slide #:title our-first-macro
         (put-at or/x
                 ct-find
                 (vl-append (t "the thing being")
                            (hbl-append (t "defined, ")
                                        (code or)))
                 's 0 30))
  (slide #:title our-first-macro
         (put-at stx/x
                 ct-find
                 (vl-append (t "defined to be a")
                            (t "function which gets")
                            (t "called to transform")
                            (hbl-append (code or)
                                        (t " expressions")))
                 's 0 30))
  (slide #:title our-first-macro
         (put-at s-p/x
                 (λ args
                   (define-values (x y) (apply ct-find args))
                   (values x (+ y 12)))
                 (vl-append (t "pattern-matching")
                            (t "for syntax (ASTs)"))
                 's 0 70))
  (slide #:title our-first-macro
         (put-at x-exp-pat/x
                 (λ args
                   (define-values (x y) (apply cb-find args))
                   (values x (- y 10)))
                 (vl-append (t "the pattern")
                            (t "to rewrite"))
                 'n 0 -70))
  (slide #:title our-first-macro
         (put-at x-exp-res/x
                 (λ args
                   (define-values (x y) (apply cb-find args))
                   (values x (- y 10)))
                 (vl-append (t "building the result")
                            (t "of the macro"))
                 'n 0 -40))

  (play-n #:title "Non-solution 2: duplicate code"
          #:skip-first? #t
          (λ (n) (scale-and-move (- 1 n)))))

(define (drop-vertical-up-to previous-slide top-size)
  (define bottom-size (- (pict-height previous-slide) top-size))
  (vc-append (cellophane (inset/clip previous-slide 0 0 0 (- bottom-size)) 0)
             (inset/clip previous-slide 0 (- top-size) 0 0)))

(module+ main (or-explanation))
