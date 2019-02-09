#lang racket

(require racket/trace)

; iterative approach
(define fibonacci-0
  (λ (n)
    (cond
      [(zero? n) 0]
      [else
       (define-values (a b tmp) (values 0 1 0))
       (for ([i (- n 1)])
         (set! tmp a)
         (set! a b)
         (set! b (+ tmp b)))
       b])))

; linear recursive
(define fibonacci-1
  (λ (n)
    (cond
      [(zero? n) 0]
      [(= n 1) 1]
      [else (+ (fibonacci-1 (- n 1)) (fibonacci-1 (- n 2)))])))

; tail recursive
(define fibonacci-aps
  (λ (a b n)
      (cond
        [(zero? n) 0]
        [(= n 1) b]
        [else (fibonacci-aps b (+ a b) (- n 1))])))

(define fibonacci-2 (λ (n) (fibonacci-aps 0 1 n)))

; tail recursive continuation passing style
(define fibonacci-cps
  (λ (n k)
    (cond
      [(zero? n) (k 0 0)]
      [(= n 1) (k 0 1)]
      [else
       (fibonacci-cps (- n 1) (λ (x y) (k y (+ x y))))])))

(define fibonacci-3 (λ (n) (fibonacci-cps n (λ (x y) y))))

; Removing recursion with Y combinator,
; we define F-fibonacci, a function having fibonacci as fixpoint.
; fibonacci = F-fibonacci(fibonacci)
(define Y
  (λ (f)
    ((λ (x) (x x))
     (λ (x) (f (λ (y) ((x x) y)))))))

(define F-fibonacci
  (λ (f)
    (λ (n)
      (cond
        [(zero? n) 0]
        [(= n 1) 1]
        [else (+ (f (- n 1)) (f (- n 2)))]))))

(define fibonacci-4 (Y F-fibonacci))


    