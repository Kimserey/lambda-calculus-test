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

(define (gcd a b)
  (let ([rest (modulo a b)])
    (if (zero? rest) b (gcd b rest))))

(define (make-rat n d) 
  (define (tag x) (cons 'rational x))
  (let ([g (gcd n d)])
    (tag (cons (/ n g) (/ d g)))))

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


(define (add-rat r1 r2)
  (if (rat? r1 r2)
      (make-rat
        (+ (* (numer r1) (denom r2)) (* (numer r2) (denom r1)))
        (* (denom r1) (denom r2)))
      (error "Values provided aren't rationals:"
             r1 r2)))

(define (sub-rat r1 r2)
  (if (rat? r1 r2)
      (make-rat
        (- (* (numer r1) (denom r2)) (* (numer r2) (denom r1)))
        (* (denom r1) (denom r2)))
      (error "Values provided aren't rationals:"
             r1 r2)))

(define (mul-rat r1 r2)
  (if (rat? r1 r2)
      (make-rat
        (* (numer r1) (numer r2))
        (* (denom r1) (denom r2)))
      (error "Values provided aren't rationals:"
             r1 r2)))

(define (div-rat r1 r2)
  (mul-rat r1 (make-rat (denom r2) (numer r1))))

