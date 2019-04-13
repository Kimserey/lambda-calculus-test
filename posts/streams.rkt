#lang racket

(define (interval low high)
  (if (> low high)
      '()
      (cons low (interval (+ low 1) high))))

(define (stream-interval low high)
  (if (> low high)
      '()
      (cons low (λ () (stream-interval (+ low 1) high)))))

(define (stream-null? xs)
  (eq? xs 'empty-stream))
(define (force expr) (expr)) 
(define (stream-car xs) (car xs))
(define (stream-cdr xs)
  (if (stream-null? xs)
      'empty-stream
      (force (cdr xs))))

(define (filter predicate xs)
  (cond [(null? xs) '()]
        [(predicate (car xs))
         (cons (car xs) (filter predicate (cdr xs)))]
        [else (filter predicate (cdr xs))]))
