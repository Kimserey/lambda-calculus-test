#lang racket

(define tolerance 0.00001)

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point
   (λ (y) (average y (/ x y)))
   10.0))

(define dx 0.00001)

(define (deriv g)
  (λ (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (λ (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g)
               guess))

; fixed-point of newton-transform is g=0
; Newton Method provides a way of better approximating the root of a function
; finding r, a value of x for g(x)=0.
; We can then use it to solve:
;       y = x^1/2
;     y^2 = x
; y^2 - x = 0
;

(define (sqrt-2 x)
  (newton-method
   (λ (y) (- (square y) x)) 1.0))


(require racket/trace)
(trace average)
