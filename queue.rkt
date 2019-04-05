#lang racket

; Mutable cons

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
(define (set-car! z v) ((z 'set-car!) v) z)
(define (set-cdr! z v) ((z 'set-cdr!) v) z)

(define (make-queue)
  (define ptrs (cons '() '()))

  (define (front-ptr) (car ptrs))
  (define (rear-ptr) (cdr ptrs))

  (define (set-front-ptr! item) (set-car! ptrs item))
  (define (set-rear-ptr! item) (set-cdr! ptrs item))

  (define (enqueue! item)
    (let ([new-pair (cons item '())])
      (cond
        [(null? (front-ptr))
         (set-front-ptr! new-pair)
         (set-rear-ptr!  new-pair)
         #t]
        [else
         (set-cdr! (rear-ptr) new-pair)
         (set-rear-ptr! new-pair)
         #t])))

  (define (dequeue!)
    (cond
      [(null? (front-ptr)) #f]
      [else
       (let ([front (front-ptr)])
         (set-front-ptr! (cdr front))
         (car front))]))
  
  (define (dispatch m)
    (cond [(eq? m 'enqueue!) enqueue!]
          [(eq? m 'dequeue!) dequeue!]))
  dispatch)

(define (enqueue! q item) ((q 'enqueue!) item))
(define (dequeue! q) ((q 'dequeue!)))

; Circuit

(define (logical-not s)
  (cond
    [(= s 0) 1]
    [(= s 1) 0]
    [else (error "Invalid signal" s)]))
    
(define (logical-and a1 a2)
  (cond
    [(and (= a1 1) (= a2 1)) 1]
    [else 0]))

(define (logical-or a1 a2)
  (cond
    [(or (= a1 1) (= a2 1)) 1]
    [else 0]))

(define (inverter input output)
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay inverter-delay (λ () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ([new-value (logical-and
                      (get-signal a1)
                      (get-signal a2))])
      (after-delay and-gate-delay (λ () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ([new-value (logical-or
                      (get-signal a1)
                      (get-signal a2))])
      (after-delay or-gate-delay (λ () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (make-wire)
  (define signal-value 0)
  (define action-procedures '())

  (define (set-my-signal! new-value)
    (if (not (= signal-value new-value))
        (begin
          (set! signal-value new-value)
          (call-each action-procedures))
        #f)
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
  dispatch)

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire proc) ((wire 'accept-action-procedure!) proc))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define inverter-delay 2)
(define and-gate-delay 4)
(define or-gate-delay 5)
(define (after-delay delay proc) 'done)