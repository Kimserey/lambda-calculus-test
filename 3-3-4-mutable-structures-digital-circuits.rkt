#lang racket

; Prelude

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond [(eq? m 'car) x]
          [(eq? m 'cdr) y]
          [(eq? m 'set-car!) set-x!]
          [(eq? m 'set-cdr!) set-y!]
          [else (error "Undefined operation: CONS" m)]))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (caar z) ((z 'car) 'car))

(define (set-car! z v)
  ((z 'set-car!) v)
  z)
  
(define (set-cdr! z v)
  ((z 'set-cdr!) v)
  z)

; Logical Gates

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

; Wire Representation

(define (make-wire)
  (let ([signal-value 0]
        [action-procedures '()])
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procedures)))
      'done)
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond
        [(eq? m 'get-signal) signal-value]
        [(eq? m 'set-signal!) set-my-signal!]
        [(eq? m 'accept-action-procedure!) accept-action-procedure!]
        [else (error "Unknown operation: WIRE" m)]))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (accept-action-procedure! wire proc)
  ((wire 'accept-action-procedure!) proc))

; Agenda

(define (after-delay delay action)
  (add-to-agenda!
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ([first-item (first-agenda-item the-agenda)])
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (cons 0 '()))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (res-segments agenda)
  (cdr (segments agenda)))

; Probes

(define (probe name wire)
  (add-action!
   wire
   (λ ()
     (newline)
     (display name)
     (display " ")
     (display (current-time the-agenda))
     (display " New-value = ")
     (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))