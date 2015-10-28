#lang racket/base
(provide hdl-read hdl-read-syntax)
(require parser-tools/lex parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         rackunit)

(define-tokens tokens (id kwd num op))
(define-empty-tokens mt-tokens (semi comma equal inputs eoft open close showint))

(define current-source (make-parameter #f))

(define lex
  (lexer-src-pos
   ["inputs" (token-inputs)]
   ["showint" (token-showint)]
   [(:or #\⊕ #\∧ #\∨) (token-op (string->symbol lexeme))]
   [#\; (token-semi)]
   [#\, (token-comma)]
   [#\= (token-equal)]
   [#\( (token-open)]
   [#\) (token-close)]
   [(:: (:/ #\a #\z #\A #\Z)
        (:* (:/ #\a #\z #\A #\Z #\0 #\9)))
    (token-id (string->symbol lexeme))]
   [(:+ (:/ #\0 #\9))
    (token-num (string->number lexeme))]
   [(:+ whitespace) (return-without-pos (lex input-port))]
   [(eof) (token-eoft)]))

(define (str->toks str)
  (let ([p (open-input-string str)])
    (let loop ()
      (let ([next (lex p)])
        (cons (token-name (position-token-token next))
              (if (eq? 'eoft (token-name (position-token-token next)))
                  '()
                  (loop)))))))


(check-equal? (str->toks "ab0 ∨  1234; inputs")
              '(id op num semi inputs eoft))
(check-equal? (str->toks "showint(x);")
              '(showint open id close semi eoft))

(define parse
  (parser
   [grammar 
    (start [(decls) $1])
    (decls [(decl) (list (add-srcloc $1 $1-start-pos $1-end-pos))]
           [(decl decls) (cons (add-srcloc $1 $1-start-pos $1-end-pos) $2)])
    (decl [(inputs ids equal num semi) `(inputs ,$2 ,$4)]
          [(inputs ids semi) `(inputs ,$2)]
          [(id equal expr semi) `(= ,(add-srcloc $1 $1-start-pos $1-end-pos #t) ,$3)]
          [(showint open ids close semi) `(showint ,@$3)])
    (expr [(expr op expr) (prec op) (add-srcloc `(,$2 ,$1 ,$3) $1-start-pos $n-end-pos)]
          [(num) (add-srcloc $1 $1-start-pos $n-end-pos)]
          [(id) (add-srcloc $1 $1-start-pos $n-end-pos #t)]
          [(open expr close) (add-srcloc $2 $1-start-pos $n-end-pos)])
    (ids [(id) (list (add-srcloc $1 $1-start-pos $1-end-pos #t))]
         [(id comma ids) (cons (add-srcloc $1 $1-start-pos $1-end-pos #t) $3)])]
   [precs (right op)]
   [tokens mt-tokens tokens]
   [src-pos]
   [start start]
   [end eoft]
   [error 
    (lambda (tok-ok? tok-name tok-value start-pos end-pos)
      (raise-syntax-error 
       'parse-error 
       (format "~s" (if tok-ok?
                        tok-value
                        'unknown))
       (add-srcloc (if tok-ok?
                       tok-value
                       'unknown)
                   start-pos
                   end-pos)))]))

(define (add-srcloc stuff start-pos end-pos [id? #f])
  (cond
    [id?
     (define str (symbol->string stuff))
     (define prt (open-input-string str))
     (port-count-lines! prt)
     (set-port-next-location! prt 
                              (position-line start-pos)
                              (position-col start-pos)
                              (position-offset start-pos))
     (read-syntax (current-source) prt)]
    [else
     (datum->syntax #f stuff 
                    (vector
                     (current-source)
                     (position-line start-pos)
                     (position-col start-pos)
                     (position-offset start-pos)
                     (- (position-offset end-pos)
                        (position-offset start-pos))))]))

(define (run-p src p)
  (parameterize ([current-source src])
    (parse (λ () (lex p)))))

(define (hdl-read [port (current-input-port)])
  (syntax->datum (run-p #f port)))

(define (hdl-read-syntax [name #f] [port (current-input-port)])
  (run-p (or name (object-name port))
         port))