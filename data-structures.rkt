#lang racket

; Stack implementation

(define (make-stack initial)
  (let ([stack (list initial)])
    (define (push x)
      (set! stack (cons x stack)))
    (define (pop)
      (if (pair? stack)
          (begin
            (set! stack (cdr stack))
            (car stack))
          (error "Stack is empty")))
    (define (dispatch m)
      (cond [(eq? m 'push) push]
            [(eq? m 'pop) pop]))
     dispatch))

(define (push x stack)
  ((stack 'push) x))
(define (pop stack)
  ((stack 'pop) stack))