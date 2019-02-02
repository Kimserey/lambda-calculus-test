#lang racket

(require (for-syntax racket/syntax))
(define-syntax (hyphen-define/ok3 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (format-id #'a "~a-~a" #'a #'b)])
        #'(define (name args ...)
                 body0 body ...))]))

(hyphen-define/ok3 foo bar () #t)