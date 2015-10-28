#lang scheme
(require "runtime.rkt" schemeunit)
(provide (except-out (all-from-out "runtime.rkt") 
                     #%module-begin
                     ∧ ∨ ⊕)
         (rename-out [module-begin #%module-begin]
                     [u:∨ ∨]
                     [u:∧ ∧]
                     [u:⊕ ⊕]))

(define u:∨
  (match-lambda* 
    [(list #t _) #t]
    [(list _ #t) #t]
    [(list #f #f) #f]
    [(list _ _) unknown]))

(define u:∧ 
  (match-lambda* 
    [(list #f _) #f]
    [(list _ #f) #f]
    [(list #t #t) #t]
    [(list _ _) unknown]))

(define u:⊕
  (match-lambda*
    [(list #t #t) #f]
    [(list #f #f) #f]
    [(list #f #t) #t]
    [(list #t #f) #t]
    [(list _ _) unknown]))

(for-each
 (λ (b1)
   (for-each
    (λ (b2)
      (check-equal? (∨ b1 b2) (u:∨ b1 b2))
      (check-equal? (∧ b1 b2) (u:∧ b1 b2))
      (check-equal? (⊕ b1 b2) (u:⊕ b1 b2)))
    '(#t #f)))
 '(#t #f))

(define unknown '?)

(define (run-example func)
  (cond
    [(equal? (procedure-arity func) 1)
     (void (func (λ (x y) (printf "~a ~a\n" x y))))]
    [else
      (let ([ht (make-hash)])
        (let loop ([i (- (procedure-arity func) 1)]
                   [bools '()])
          (cond
            [(zero? i)
             (let ([depth (+ 1 (apply func void bools))]) ;; need to add one here to count the step that seeds the inputs
               (hash-set! ht depth (cons bools (hash-ref ht depth '()))))]
            [else 
             (loop (- i 1) (cons #t bools))
             (loop (- i 1) (cons #f bools))]))
        (let ([vals (sort (hash-map ht list) < #:key car)])
          (for-each
           (λ (val)
             (printf "~a inputs have gate delay ~a, e.g. ~s\n" 
                     (length (list-ref val 1))
                     (list-ref val 0)
                     (map (λ (x) (if x 1 0)) (car (list-ref val 1)))))
           vals)))]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ a ...) 
     (let* ([vars '()]
            [input-vars '()]
            [let-frames 
             (map (λ (x) 
                    (let ([ans (local-expand x 'module-begin (list #'#%app #'define-values #'inputs))])
                      (syntax-case ans (define-values inputs)
                        [(inputs these-vars) 
                         (set! input-vars (append (reverse (syntax->list #'these-vars)) input-vars))
                         #'[() (values)]]
                        [(inputs these-vars exp) 
                         (set! vars (append (reverse (syntax->list #'these-vars)) vars))
                         (with-syntax ([(i ...) 
                                        (let ([count (length (syntax->list #'these-vars))])
                                          (build-list count (λ (x) (- count x 1))))])
                           #'[these-vars (let ([n exp]) (values (extract-bit i n) ...))])]
                        [(define-values these-vars exp)
                         (set! vars (append (reverse (syntax->list #'these-vars)) vars))
                         #'[these-vars exp]]
                        [exp
                         #'[() (begin exp (values))]])))
                  (syntax->list #'(a ...)))])
       (with-syntax ([(vars ...) (reverse vars)]
                     [(input-vars ...) (reverse input-vars)])
         (with-syntax ([(old-vars ...) (generate-temporaries #'(vars ...))]
                       [(let-frames ...) let-frames])
           #'(#%module-begin 
              (run-example
               (λ (details input-vars ...)
                 (let loop ([i 0] [vars unknown] ...)
                   (details i (append (list 'vars (if (symbol? vars) vars (if vars 1 0))) ...))
                   (let ([old-vars vars] ...)
                     (let-values (let-frames ...)
                       (cond [(equal? (list old-vars ...) (list vars ...))
                              i]
                             [else
                              (loop (+ i 1) vars ...)]))))))))))]))
