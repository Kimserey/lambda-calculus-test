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
         
(define operation-table '())

(define (get-operation name tags)
  (define (get ops)
    (cond
      [(null? ops) #f]
      [(and
        (equal? name (caar ops))
        (equal? tags (cadar ops)))
       (caddar ops)]
      [else (get (cdr ops))]))
  (get operation-table))

(define (install-operation name tags operation)
  (set! operation-table (cons (list name tags operation) operation-table)))

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
  (install-operation 'make 'num make)
  (install-operation 'add '(num num) add)
  (install-operation 'sub '(num num) sub)
  (install-operation 'mul '(num num) mul)
  (install-operation 'div '(num num) div)
  'done)

(define (make-num x) ((get-operation 'make 'num) x))

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

  ; populate operation-table
  (install-operation 'make 'rational make)
  (install-operation 'add '(rational rational) add)
  (install-operation 'sub '(rational rational) sub)
  (install-operation 'mul '(rational rational) mul)
  (install-operation 'div '(rational rational) div)
  'done)

(define (make-rat n d) ((get-operation 'make 'rational) n d))

; coercion

(define coercion-table '())

(define (get-coercion t1 t2)
  (define (get ops)
    (cond
      [(null? ops) #f]
      [(and
        (eq? t1 (caar ops))
        (eq? t2 (cadar ops)))
       (caddar ops)]
      [else (get (cdr ops))]))
  (get coercion-table))

(define (install-coercion t1 t2 operation)
  (set! coercion-table (cons (list t1 t2 operation) coercion-table)))

(define (install-coercion-package)
  (define (number->rational n)
    (make-rat n 1))
  
  ; populate coercion-table
  (install-coercion 'number 'rational number->rational)
  'done)

; generic operations

(define (apply-operation name . args)
  (let* ([tags (map get-tag args)]
         [op (get-operation name tags)])
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


