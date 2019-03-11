#lang racket


(define (num-type? type . xs)
  (if (null? xs)
      #t
      (and (pair? (car xs))
           (equal? type (caar xs))
           (apply num-type? type (cdr xs)))))

; numbers

(define (make-num x)
  (define (tag x) (cons 'num x))
  (tag x))

(define (value x)
  (if (pair? x)
      (cdr x)
      (error "Value provided isn't tagged:"
             x)))

(define (num? . xs)
  (apply num-type? 'num xs))
  
(define (add x y) 
  (if (num? x y)
      (make-num (+ (value x) (value y)))
      (error "Values provided aren't numbers of type 'num:"
             x y)))

(define (sub x y) 
  (if (num? x y)
      (make-num (- (value x) (value y)))
      (error "Values provided aren't numbers of type 'num:"
             x y)))

(define (mul x y) 
  (if (num? x y)
      (make-num (* (value x) (value y)))
      (error "Values provided aren't numbers of type 'num:"
             x y)))

(define (div x y) 
  (if (num? x y)
      (make-num (/ (value x) (value y)))
      (error "Values provided aren't numbers of type 'num:"
             x y)))

; rationals

(define (make-rat n d) 
  (define (tag x) (cons 'rational x))
  (tag (cons n d)))

(define (rat? . xs)
  (apply num-type? 'rational xs))

(define (numer x)
  (if (rat? x)
      (car (value x))
      (error "Value provided isn't a rational number:"
             x)))

(define (denom x)
  (if (rat? x)
      (cdr (value x))
      (error "Value provided isn't a rational number:"
             x)))