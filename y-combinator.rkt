#lang racket

(require racket/trace)

(define factorial
  (λ (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

(trace factorial)
(factorial 5)