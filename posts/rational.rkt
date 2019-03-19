#lang racket

(define (gcd a b)
  (let ([rest (modulo a b)])
    (if (zero? rest) b (gcd b rest))))

(define (num-type? type . xs)
  (if (null? xs)
      #t
      (and (pair? (car xs))
           (equal? type (caar xs))
           (apply num-type? type (cdr xs)))))

(define (get-tag x)
  (if (pair? x)
      (car x)
      (error "Value provided is not tagged:"
             x)))

(define (value x)
  (if (pair? x)
      (cdr x)
      (error "Value provided isn't tagged:"
             x)))
         
(define op-table '())

(define (get-op name tags)
  (define (get ops)
    (cond
      [(null? ops) #f]
      [(and
        (equal? name (caar ops))
        (equal? tags (cadar ops)))
       (caddar ops)]
      [else (get (cdr ops))]))
  (get op-table))

(define (install name tags operation)
  (set! op-table (cons (list name tags operation) op-table)))

; numbers

(define (install-number-package)
  ; prelude procedures
  (define (num? . xs)
    (apply num-type? 'num xs))

  ; constructor
  (define (make x)
    (define (tag x) (cons 'num x))
    (tag x))

  ; operations
  (define (add x y) 
    (if (num? x y)
        (make (+ (value x) (value y)))
        (error "Values provided aren't numbers of type 'num:"
               x y)))

  (define (sub x y) 
    (if (num? x y)
        (make (- (value x) (value y)))
        (error "Values provided aren't numbers of type 'num:"
               x y)))

  (define (mul x y) 
    (if (num? x y)
        (make (* (value x) (value y)))
        (error "Values provided aren't numbers of type 'num:"
               x y)))

  (define (div x y) 
    (if (num? x y)
        (make (/ (value x) (value y)))
        (error "Values provided aren't numbers of type 'num:"
               x y)))

  ; populte op-table
  (install 'make 'num make)
  (install 'add '(num num) add)
  (install 'sub '(num num) sub)
  (install 'mul '(num num) mul)
  (install 'div '(num num) div)
  'done)

(define (make-num x) ((get-op 'make 'num) x))

; rationals

(define (install-rational-package)
  ; prelude procedures
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

  ; constructor
  (define (make n d) 
    (define (tag x) (cons 'rational x))
    (let ([g (gcd n d)])
      (tag (cons (/ n g) (/ d g)))))

  ; operations
  (define (add r1 r2)
    (if (rat? r1 r2)
        (make (+ (* (numer r1) (denom r2)) (* (numer r2) (denom r1)))
              (* (denom r1) (denom r2)))
        (error "Values provided aren't rationals:"
               r1 r2)))

  (define (sub r1 r2)
    (if (rat? r1 r2)
        (make (- (* (numer r1) (denom r2)) (* (numer r2) (denom r1)))
              (* (denom r1) (denom r2)))
        (error "Values provided aren't rationals:"
               r1 r2)))

  (define (mul r1 r2)
    (if (rat? r1 r2)
        (make (* (numer r1) (numer r2))
              (* (denom r1) (denom r2)))
        (error "Values provided aren't rationals:"
               r1 r2)))

  (define (div r1 r2)
    (mul r1 (make (denom r2) (numer r1))))

  ; populate op-table
  (install 'make 'rational make)
  (install 'add '(rational rational) add)
  (install 'sub '(rational rational) sub)
  (install 'mul '(rational rational) mul)
  (install 'div '(rational rational) div)
  'done)

(define (make-rat n d) ((get-op 'make 'rational) n d))

; generic operations

(define (apply-operation name . args)
  (let* ([tags (map get-tag args)]
         [op (get-op name tags)])
    (if op
        (apply op args)
        (error "No operation found for:"
               name args tags))))

(define (add x y) (apply-operation 'add x y))
(define (sub x y) (apply-operation 'sub x y))
(define (mul x y) (apply-operation 'mul x y))
(define (div x y) (apply-operation 'div x y))

; install

(install-number-package)
(install-rational-package)