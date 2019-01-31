#lang racket

; http://matt.might.net/articles/compiling-up-to-lambda-calculus/

; lambda-calculus
; <exp> ::= <var>
;         |  (<exp> <exp>)
;         |  (λ(<var>) <exp>)


; <exp> ::= <var>
;
;        |  #t
;        |  #f
;        |  (if  <exp> <exp> <exp>)
;        |  (and <exp> <exp>)
;        |  (or  <exp> <exp>)
;
;        |  <nat>
;        |  (zero? <exp>)
;        |  (- <exp> <exp>)
;        |  (= <exp> <exp>)
;        |  (+ <exp> <exp>)
;        |  (* <exp> <exp>)
;
;        |  <lam>
;        |  (let ((<var> <exp>) ...) <exp>)
;        |  (letrec ((<var> <lam>)) <exp>)

;        |  (cons <exp> <exp>)
;        |  (car  <exp>)
;        |  (cdr  <exp>)
;        |  (pair? <exp>)
;        |  (null? <exp>)
;        |  '()
;
;        |  (<exp> <exp> ...)
;
; <lam> ::= (λ (<var> ...) <exp>)

(define (match-test value)
  (match value
    [`(list ,x ,y) (printf "~a ~a" x y)]
    [_ (displayln "nothing")]))

(match-test '(list 1 2))