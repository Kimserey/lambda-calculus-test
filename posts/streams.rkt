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

(define (take n xs )
  (cond [(= n 0) '()]
        [else (cons (car xs) (take (- n 1) (cdr xs)))]))

(define (take-stream n xs)
  (cond [(= n 0) 'empty-stream]
        [else (cons (stream-car xs)
                    (λ () (take-stream (- n 1) (stream-cdr xs))))]))

(define (for-each-stream proc xs)
  (if (stream-null? xs)
      'done
      (begin
        (proc (stream-car xs))
        (for-each-stream proc (stream-cdr xs)))))


(define (integers-starting-from n)
  (cons n (λ () (integers-starting-from (+ n 1)))))

(define (fib-stream a b)
  (cons a (λ () (fib-stream b (+ a b)))))

(define fibonacci (fib-stream 0 1))

(define (stream-ref n xs)
  (cond
    [(stream-null? xs) 'empty-stream]
    [(= n 0) (car xs)]
    [else (stream-ref (- n 1) (stream-cdr xs))]))