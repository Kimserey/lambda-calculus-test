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



(define (make-wire)
  #f)

;(define (make-wire)
;  (let ([signal-value 0]
;        [action-procedures '()])
;
;    (define (set-my-signal! new-value)
;      (if (not (= signal-value new-value))
;          (begin (set! signal