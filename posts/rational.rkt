#lang racket

(define (tag x) (cons 'num x))

(define (value x)
  (if (num? x)
      (cdr x)
      (error "Value provided isn't of type 'num:"
             x)))

(define (num? . xs)
  (if (null? xs)
      #t
      (and (pair? (car xs))
           (equal? 'num (caar xs))
           (apply num? (cdr xs)))))

(define (add x y) 
  (if (num? x y)
      (tag (+ (value x) (value y)))
      (error "Values provided aren't numbers of type 'num:"
             x y)))

(define (sub x y) 
  (if (num? x y)
      (tag (- (value x) (value y)))
      (error "Values provided aren't numbers of type 'num:"
             x y)))

(define (mul x y) 
  (if (num? x y)
      (tag (* (value x) (value y)))
      (error "Values provided aren't numbers of type 'num:"
             x y)))

(define (div x y) 
  (if (num? x y)
      (tag (/ (value x) (value y)))
      (error "Values provided aren't numbers of type 'num:"
             x y)))