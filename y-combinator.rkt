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

; The Y none combinator - as it references itself in a recursive manner
(define Y
  (λ (f)
    (f (λ (x) (Y f) x))))

(define factorial/2 (Y almost-factorial))

; Deriving the Y combinator

(define (part-factorial self)
  (let ((f (lambda (y) ((self self) y))))
    (lambda (n)
      (if (= n 0)
          1
          (* n (f (- n 1)))))))

(define (part-factorial/2 self)
  (almost-factorial
   (λ (y) ((self self) y))))

(define part-factorial/3
  (λ (self)
    (almost-factorial
     (λ (y) ((self self) y)))))

(define factorial/3 (part-factorial/2 part-factorial/2))

(define factorial/4
  (let ([pf (λ (self)
              (almost-factorial
               (λ (y) ((self self) y))))])
    (pf pf)))

(define factorial/5
  ((λ (x) (x x))
   (λ (x)
     (almost-factorial (λ (y) ((x x) y))))))

; Extracts the pattern within factorial/5 to make a general recursive pattern
(define make-recursive
  (λ (f)
    ((λ (x) (x x))
     (λ (x) (f (λ (y) ((x x) y)))))))

(define Y-combinator
  (λ (f)
    ((λ (x) (x x))
     (λ (x) (f (λ (y) ((x x) y)))))))

(define factorial/6 (Y-combinator almost-factorial))