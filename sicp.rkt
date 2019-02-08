#lang racket
(require racket/trace)

; A function f is defined by the rule that f(n)=n if n<3
; and f(n)=f(n-1)+2f(n-2)+3f(n-3) if n≥3.

(define (f n)
  (cond
    [(< n 3) n]
    [else (+ (+ (f (- n 1)) (* 2 (f (- n 2)))) (* 3 (f (- n 3))))]))

; (trace f)

(define (f-aps n a b c)
  (cond
    [(< n 3) n]
    [(= n 3)
     (+ c (+ (* 2 b) (* 3 a)))]
    [else
     (f-aps (- n 1) b c (+ c (+ (* 2 b) (* 3 a))))]))

(define (f-2 n)
  (f-aps n 0 1 2))

(trace f-aps)

(f 8)
(f-2 8)