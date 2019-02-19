#lang racket

(define espilon 0.00001)

(define (almost-equal? v1 v2) (< (abs (- v1 v2)) espilon))

(define (fixed-point f initial-guess)
  (define (apply x) 
    (let ([ result (f x)])
      (if (almost-equal? x result)
        result
        (apply result))))
  (apply initial-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (sqrt x)
  (fixed-point
   (λ (y) (average y (/ x y)))
   10.0))

(define dx 0.00001)

(define (deriv g)
  (λ (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (average-damp f)
  (λ (x)
    (average x (f x))))

(define (newton-transform g)
  (λ (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))


(define (sqrt-3 x)
  (fixed-point-of-transform
   (λ (y) (/ x y))
   average-damp
   1.0))
   
(define (sqrt-nm x)
  (fixed-point-of-transform
   (λ (y) (- x (square y)))
   newton-transform
   1.0))
  

; fixed-point of newton-transform is g=0

; Newton Method provides a way of better approximating the root of a function.
; Better approximation using:
; f(x) = x - g(x)/g'(x)


; y = sqrt(x)
; 0 = x - y^2
; f(y) = x - y^2 or f(y) = y^2 - x


; x - (x - y^2) / 2y
;


; Finding r, a value of x for g(x)=0.
; We can then use it to solve:
;   y = x^1/2
; y^2 = x
;   0 = x - y^2
; https://en.wikipedia.org/wiki/Newton%27s_method

(define (sqrt-2 x)
  (newton-method
   (λ (y) (- x (square y))) 1.0))


(require racket/trace)
(trace average)

(trace fixed-point)
(fixed-point cos 1.0)
