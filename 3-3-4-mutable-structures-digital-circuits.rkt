#lang racket


(define (half-adder a b s c)
  (let ([d (make-wire)]
        [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ([c1 (make-wire)]
        [c2 (make-wire)]
        [s (make-wire)])
    (half-adder a s sum c1)
    (half-adder b c-in s c2) 
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay inverter-delay (λ () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond
    [(= s 0) 1]
    [(= s 1) 0]
    [else (error "Invalid signal" s)]))

(define (and-gate a1 a2 ouput)
  (define (and-action-procedure)
    (let ([new-value (logical-and
                      (get-signal a1)
                      (get-signal a2))])
      (after-delay and-gate-delay (λ () (set-signal! output new-value)))))
  (add-action! input and-action-procedure)
  'ok)

(define (logical-and a1 a2)
  (cond
    [(and (= a1 1) (= a2 1)) 1]
    [else 0]))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ([new-value (logical-or
                      (get-signal a1)
                      (get-signal a2))])
      (after-delay or-gate-delay (λ () (set-signal! output new-value)))))
  (add-action! input or-action-procedure)
  'ok)

(define (logical-or a1 a2)
  (cond
    [(or (= a1 1) (= a2 1)) 1]
    [else 0]))

(define (make-wire)
  #f)

;(define (make-wire)
;  (let ([signal-value 0]
;        [action-procedures '()])
;
;    (define (set-my-signal! new-value)
;      (if (not (= signal-value new-value))
;          (begin (set! signal