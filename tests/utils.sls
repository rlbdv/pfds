#!r6rs
(library (pfds tests utils)
(export pfds
        define-test-case
        define-test-suite
        test
        test-case
        test-compare
        test-not
        test-end
        test-equal
        test-eqv
        test-predicate
        test-exn
        test-no-exn
        add1
        foldl
        iota
        )
(import (rnrs)
        (srfi :64))

(define-syntax define-test-suite
  (syntax-rules ()
    ((_ name desc)
     (test-begin (string-append (symbol->string 'name) ": " desc)))))

(define-syntax test-case
  (syntax-rules ()
    ((_ desc _ body ...)
     (test-group (string-append (symbol->string 'desc)) body ...))))

(define-syntax define-test-case
  (syntax-rules ()
    ((_ _ desc () body ...) (define-test-case _ desc body ...))
    ((_ _ desc body ...)
     (test-group (string-append (symbol->string 'desc)) body ...))))

(define-syntax test
  (syntax-rules ()
    ((test body)
     (test-eqv #t (and body #t)))))

(define-syntax test-compare
  (syntax-rules ()
    ((_ pred expected actual)
     (test-assert (pred expected actual)))))

(define-syntax test-not
  (syntax-rules ()
    ((test-not body)
     (test-eqv #f body))))

(define-syntax test-predicate
  (syntax-rules ()
    ((_ pred item) (test-assert (pred item)))))

(define-syntax test-exn
  (syntax-rules ()
    ((test-exn exception-pred? body)
     (test-eqv #t
               (guard (exn ((exception-pred? exn) #t)
                           (else #f))
                 body
                 #f)))))

(define-syntax test-no-exn
  (syntax-rules ()
    ((test-no-exn body)
     (test-eqv #t
               (guard (exn (else #f))
                 body
                 #t)))))

(define (add1 x)
  (+ x 1))

(define (foldl kons knil list)
  (if (null? list)
      knil
      (foldl kons (kons (car list) knil) (cdr list))))

(define (iota n)
  (define (recur x)
    (if (< x n)
        (cons x (recur (+ x 1)))
        '()))
  (assert (integer? n))
  (recur 0))

)
