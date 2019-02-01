#lang racket

; Define a syntax
(syntax (lambda (k) k))
#'(+ 1 2)
#'"hello"

; quote
'(lambda (k) k)
'(+ 1 2)
'"hello"

; syntax transformer

(define-syntax foo
  (lambda (stx)
    #'(displayln "I am foo")))
(foo)

(define-syntax (foo-two stx) #'"I am foo 2")
(foo-two)

; Composition of a syntax is the source location and the quoted S-expression
(define-syntax (show-me stx)
  (print stx)
  #'"hello")
(show-me '(+ 1 12))

(define stx #'(+ 1 (+ 2 (+ 3 3))))
stx
(syntax-source stx)
(syntax-line stx)
(syntax-column stx)
; return datum from syntax (quote)
(syntax->datum stx)
; goes one level down and return list of syntax objects
(syntax-e stx)
(syntax->list stx)