#lang racket

; linear recursive
(define fibonacci-1
  (λ (n)
    (cond
      [(zero? n) 0]
      [(= n 1) 1]
      [else (+ (fibonacci-1 (- n 1)) (fibonacci-1 (- n 2)))])))

; linear iterative 
(define fibonacci-aps
  (λ (a b n)
      (cond
        [(zero? n) 0]
        [(= n 1) b]
        [else (fibonacci-aps b (+ a b) (- n 1))])))

(define fibonacci-2 (λ (n) (fibonacci-cps 0 1 n)))

; linear iterative recursion continuation passing style
(define fibonacci-cps
  (λ (n k)
    (cond
      [(zero? n) (k 0 0)]
      [(= n 1) (k 0 1)]
      [else
       (fibonacci-cps-a (- n 1) (λ (x y) (k y (+ x y))))])))

(define fibonacci-3 (λ (n) (fibonacci-cps n (λ (x y) y))))
    