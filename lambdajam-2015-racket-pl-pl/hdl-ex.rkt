#lang racket
(require slideshow slideshow/code 
         racket/runtime-path
         "util.ss"
         mini-hdl/parser)

(provide hdl-ex)

(define-runtime-path mini-hdl/info.rkt '(lib "info.rkt" "mini-hdl"))
(define mini-hdl
  (let-values ([(base name dir?) (split-path mini-hdl/info.rkt)])
    base))

(define title "Working through mini-hdl")

(define (count-lines file)
  (call-with-input-file file
    (λ (port)
      (let loop ([i 0])
        (let ([l (read-line port 'any)])
          (cond
            [(eof-object? l) i]
            [else (loop (+ i 1))]))))))

(define parser (build-path mini-hdl "parser.rkt"))
(define parser-contents (contents-of parser))
(define parser-lines (count-lines parser))

(define runtime (build-path mini-hdl "runtime.rkt"))
(define runtime-contents (contents-of runtime))
(define runtime-lines (count-lines runtime))

(define gc-runtime (build-path mini-hdl "gc-runtime.rkt"))
(define gc-runtime-contents (contents-of gc-runtime))
(define gc-runtime-lines (count-lines gc-runtime))

(define rca (build-path mini-hdl "rca-2-hdl.rkt"))
(define rca-contents (contents-of rca #t))

(define all-code (list parser-contents runtime-contents gc-runtime-contents))
(define all-lines (list parser-lines runtime-lines gc-runtime-lines))

(define desired-width 200)
(define code-scale
  (/ desired-width (apply max (map pict-width all-code))))

(define (format-line x)
  (match x
    [`(define c1 (,op ,a ,b))
     (vl-append (tt (format "(define c1 (~s ~s" op a))
                (tt (format "              ~s))" b)))]
    [`(= c1 (,op ,a ,b))
     (vl-append (tt (format "(= c1 (~s ~s" op a))
                (tt (format "         ~s))" b)))]
    [`(c1 (,op ,a ,b))
     (vl-append (tt (format "(c1 (~s ~s" op a))
                (tt (format "       ~s))" b)))]
    [_
     (tt (format "~s" x))]))

(define rca-sexps
  (map syntax->datum
       (call-with-input-file rca
         (λ (port) (read-line port)
           (hdl-read-syntax (object-name port) port)))))

(define (rewrite-all x)
  (let loop ([x x])
    (let ([next (flatten-begins (map rewrite x))])
      (cond
        [(equal? next x) x]
        [else (loop next)]))))

(define (rewrite x)
  (match x
    [`(= ,a ,b) `(define ,a ,b)]
    [`(define ,x ,a) `(define ,x ,(rewrite a))]
    [`(inputs () ,a) `(begin)]
    [`(inputs (,fst ,rst ...) ,x)
     (let ([a (last (cons fst rst))]
           [b (reverse (cdr (reverse (cons fst rst))))])
       `(begin (define ,fst (nth-bit ,(length b) ,x))
               (inputs ,rst ,x)))]
    [_ x]))

(define (flatten-begins lst)
  (cond
    [(null? lst) null]
    [else
     (append  (match (car lst)
                [`(begin ,a ...)
                 (flatten-begins a)]
                [_ (list (car lst))])
              (flatten-begins (cdr lst)))]))

(define rca-parens
  (apply vl-append
         (map format-line
              rca-sexps)))

(define rca-rewrite1
  (apply vl-append
         (map format-line
              (rewrite-all rca-sexps))))

(define rca-rewrite2
  (let ([defines
          (filter (λ (x) (and (pair? x) (eq? (car x) 'define)))
                  (rewrite-all rca-sexps))])
    (let-values ([(fst-names snd-names) (split-at (map (λ (x) (tt (format "~a" (list-ref x 1))))
                                                       defines)
                                                  (quotient (length defines) 2))])
      (let ([fst-names-pict (apply hbl-append (add-between fst-names (tt " ")))]
            [snd-names-pict (apply hbl-append (add-between snd-names (tt " ")))])
        (vl-append
         (hbl-append (tt "(define (iterate ") fst-names-pict)
         (hbl-append (tt "                 ") snd-names-pict (tt ")"))
         (htl-append (tt " (let (")
                     (apply vl-append
                            (map (λ (x) 
                                   (format-line `[,(list-ref x 1)
                                                  ,(list-ref x 2)]))
                                 defines)))
         (hbl-append (tt (format "  (values "))
                     fst-names-pict)
         (hbl-append (tt (format "          "))
                     snd-names-pict
                     (tt "))))")))))))
  
(define (code+lines title contents lines)
  (let ([main (scale contents code-scale)]) 
    (vl-append 
     main
     (t title)
     (t (format "~a lines" lines)))))

(define (stage stage1 stage2 stage3 stage4 stage5 stage6 stage7)
  (slide
   #:title title
   (vl-append
    (ht-append
     (lt-superimpose (stage1 (ht-append (ghost (tt "(")) rca-contents))
                     (stage2 rca-parens)
                     (stage3 (vl-append rca-rewrite1
                                        (colorize (hbl-append goes-to (tt (format "3"))) "red")))
                     (stage4 rca-rewrite2))
     (blank 100 0)
     (lt-superimpose (stage5 (code+lines "parser.rkt" parser-contents parser-lines))
                     (stage6 (code+lines "runtime.rkt" runtime-contents runtime-lines))
                     (stage7 (code+lines "gc-runtime.rkt" gc-runtime-contents gc-runtime-lines)))))))

(define (code-from-file pth)
  (call-with-input-file (build-path mini-hdl pth)
    (λ (port)
      (port-count-lines! port)
      (vl-append
       (tt (read-line port))
       (typeset-code
        (datum->syntax
         #f
         (cons 'code:line
               (let loop ()
                 (let ([l (read-syntax 'whatever port)])
                   (cond
                     [(eof-object? l) '()]
                     [else (cons l (loop))]))))
         #f #f))))))

(define glue-code
  (let ([top (code-from-file "gc/lang/reader.rkt")]
        [bottom (code-from-file "lang/reader.rkt")]) 
    (scale
     (vc-append
      40
      top
      (frame (blank (+ 40 (pict-width top)) 0))
      bottom)
     1)))

(define (hdl-ex)
  (stage values ghost ghost ghost ghost ghost ghost)
  (stage ghost values ghost ghost values ghost ghost)
  (stage ghost ghost values ghost ghost values ghost)
  (stage ghost ghost ghost values ghost ghost values)
  (slide #:title title glue-code))

(module+ slideshow
  (hdl-ex))
