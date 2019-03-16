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
      (set! queue (append queue x)))

    (define (dequeue)
      (if (pair? queue)
          (let ([x (car queue)])
            (set! queue (cdr queue))
            x)
          #f))
    
    (define (dispatch m)
      (cond [(eq? m 'enqueue) enqueue]
            [(eq? m 'dequeue) dequeue]))
     dispatch))

(define (enqueue! queue x)
  ((queue 'enqueue) x))
(define (dequeue! queue)
  ((queue 'dequeue)))

; Set implementation

(define (make-set)
  (let ([content '()])

    (define (adjoin x)
      (define (adjoin xs x)
        (if (null? xs)
            (list x)
            (cond
              [(= x (car xs)) xs]
              [(< x (car xs)) (cons x xs)]
              [else (cons (car xs) (adjoin (cdr xs) x))])))
      (set! content (adjoin content x)))

    (define (dispatch m)
      (cond [(eq? m 'adjoin) adjoin]
            [(eq? m 'content) content]))
    dispatch))

(define (adjoin! set x)
  ((set 'adjoin) x))
(define (content set)
  (set 'content))