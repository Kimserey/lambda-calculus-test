#lang racket

; code from mvanier post:
; https://mvanier.livejournal.com/2897.html

(require racket/trace)

(define factorial
  (λ (n)
    (if (zero? n)
        1
        (* n (factorial (- n 1))))))

; (trace factorial)
; (factorial 5)

(define almost-factorial
  (λ (f)
    (λ (n)
      (if (zero? n)
          1
          (* n (f (- n 1)))))))

;(define factorial (Y almost-factorial))
; with hypothetical factorial working

(define identity (λ (x) x))

(define factorialB (almost-factorial identity))

(define factoiralB/1
  (lambda (n)
    (if (zero? n)
        1
        (* n (identity (- n 1))))))

(define factorial0 (almost-factorial identity))

;(factorial0 1)

(define factorial1
  (almost-factorial factorial0))

(define factorial1/2
  (almost-factorial
   (almost-factorial identity)))

; factorial is the fixpoint of almost-factorial because:
; factorial = almost-factorial factorial
; as almost-factorial takes a function which is expected to be a "working" factorial,
; if we pass factorial, it will work since factorial is definitely working.

(define Y
  (λ (f)
    (f (λ (x) (Y f) x))))

(define factorial/2 (Y almost-factorial))