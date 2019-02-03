#lang racket

(require racket/match)

(define (eval exp env)
  (match exp
    [`(λ ,v . ,e)   `(closure ,exp ,env)]
    [`(,f ,e)        (apply (eval f env) (eval e env))]
    [(? symbol?)     (cadr (assq exp env))]))

(define (apply f x) (match f
  [`(closure (λ ,v . ,body) ,env)
    (eval body (cons `(,v ,x) env))]))

(display (eval (read) '()))
(newline)