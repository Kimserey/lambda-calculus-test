#lang racket

; Prelude

(define (append ls x)
  (if (null? ls)
      (list x)
      (cons (car ls) (append (cdr ls) x))))

; Stack implementation

(define (make-stack initial)
  (let ([stack (list initial)])

    (define (push x)
      (set! stack (cons x stack)))
    
    (define (pop)
      (if (pair? stack)
          (let ([x (car stack)])
            (set! stack (cdr stack))
            x)
          #f))
    
    (define (dispatch m)
      (cond [(eq? m 'push) push]
            [(eq? m 'pop) pop]))
     dispatch))

(define (push x stack)
  ((stack 'push) x))
(define (pop stack)
  ((stack 'pop) stack))

; Queue implementation

(define (make-queue)
  (let ([queue '()])
    
    (define (enqueue x)
      (set! queue (cons x queue)))
    
    (define (dequeue)
      (define (pop-last new-queue rest)
        (cond
          [(null? rest) #f]
          [(null? (cdr rest))
           (let ([last (car rest)])
             (set! queue new-queue)
             last)]
          [else (pop-last (append new-queue (car rest)) (cdr rest))]))
      (pop-last '() queue))
    
    (define (dispatch m)
      (cond [(eq? m 'enqueue) enqueue]
            [(eq? m 'dequeue) dequeue]))
     dispatch))

(define (enqueue! queue x)
  ((queue 'enqueue) x))
(define (dequeue! queue)
  ((queue 'dequeue)))