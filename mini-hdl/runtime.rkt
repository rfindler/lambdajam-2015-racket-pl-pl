#lang racket
(require rackunit)

(define-syntax-rule (= a b) (define a b))

(define (∧ a b) (and a b))
(define (∨ a b) (or a b))
(define (⊕ a b) (not (equal? a b)))

(define-syntax (inputs stx)
  (syntax-case stx ()
    [(inputs (a ...))
     (raise-syntax-error 'inputs "expected an initial value" stx)]
    [(inputs (a ...) n-exp)
     (with-syntax ([(i ...) 
                    (let ([count (length (syntax->list #'(a ...)))])
                      (build-list count (λ (x) (- count x 1))))])
       #'(define-values (a ...) 
           (let ([n n-exp])
             (values (extract-bit i n) ...))))]))

;; extract-bit : number[index] number[bits] -> boolean
;; returns the ith bit in n (indexing from least significant)
(define (extract-bit i n)
  (let ([bits (n->bits n)])
    (cond
      [(< i (length bits))
       (list-ref bits i)]
      [else #f])))

;; n->bits : positive-integer -> (listof boolean)
;; returns the bits of n from least significant to most significant
(define (n->bits n)
  (cond
    [(zero? n) '()]
    [(zero? (modulo n 2)) (cons #f (n->bits (quotient n 2)))]
    [else (cons #t (n->bits (quotient n 2)))]))

(check-equal? (n->bits 1) (list #t))
;; 11 = 1 + 2 + 8
(check-equal? (n->bits 11) (list #t #t #f #t))
(check-equal? (extract-bit 0 1) #t)
(check-equal? (extract-bit 0 0) #f)
(check-equal? (extract-bit 0 11) #t)
(check-equal? (extract-bit 2 11) #f)
(check-equal? (extract-bit 111 11) #f)
(check-equal? (let ()
                (inputs (a3 a2 a1 a0) 7)
                (list a3 a2 a1 a0))
              (list #f #t #t #t))

(define (showint . args)
  (let loop ([args args]
             [n 0])
    (cond
      [(null? args) n]
      [else 
       (loop (cdr args)
             (+ (* n 2)
                (if (car args) 1 0)))])))

(check-equal? (showint #t #t #f #t) 13)

(provide
 #%module-begin
 #%datum
 #%app
 #%top-interaction
 #%top
 extract-bit
 showint
 inputs
 = ⊕ ∧ ∨)
