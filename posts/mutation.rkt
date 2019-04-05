#lang racket

(define fibonacci
  (λ (n)
    (cond
      [(zero? n) 0]
      [(= 1 n) 1]
      [else
       (define-values (a b tmp) (values 0 1 0))
       (for ([i (- n 1)])
         (set! tmp a)
         (set! a b)
         (set! b (+ tmp b)))
       b])))

(define (add-fib 3 4) 
  (+ (fibonacci 3) (fibonacci 4)))