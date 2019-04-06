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

  (define (empty-queue?) (null? (front-ptr)))
  
  (define (enqueue! item)
    (let ([new-pair (cons item '())])
      (cond
        [(empty-queue?)
         (set-front-ptr! new-pair)
         (set-rear-ptr!  new-pair)
         #t]
        [else
         (set-cdr! (rear-ptr) new-pair)
         (set-rear-ptr! new-pair)
         #t])))

  (define (dequeue!)
    (cond
      [(empty-queue?) #f]
      [else
       (let ([front (front-ptr)])
         (set-front-ptr! (cdr front))
         (car front))]))
  
  (define (peek)
    (if (empty-queue?)
        #f
        (car (front-ptr))))

  (define (dispatch m)
    (cond [(eq? m 'enqueue!) enqueue!]
          [(eq? m 'dequeue!) dequeue!]
          [(eq? m 'peek) peek]
          [(eq? m 'empty-queue?) empty-queue?]))
  dispatch)

(define (enqueue! q item) ((q 'enqueue!) item))
(define (dequeue! q) ((q 'dequeue!)))
(define (peek q) ((q 'peek)))
(define (empty-queue? q) ((q 'empty-queue?)))

; Circuit
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (inverter input output agenda)
  (define (logical-not s)
    (cond
      [(= s 0) 1]
      [(= s 1) 0]
      [else (error "Invalid signal" s)]))
  
  (define (invert-input)
    (let ([new-value (logical-not (get-signal input))])
      (after-delay agenda inverter-delay (λ () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output agenda)
  (define (logical-and a1 a2)
    (cond
      [(and (= a1 1) (= a2 1)) 1]
      [else 0]))

  (define (and-action-procedure)
    (let ([new-value (logical-and
                      (get-signal a1)
                      (get-signal a2))])
      (after-delay agenda and-gate-delay (λ () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output agenda)
  (define (logical-or a1 a2)
    (cond
      [(or (= a1 1) (= a2 1)) 1]
      [else 0]))
  
  (define (or-action-procedure)
    (let ([new-value (logical-or
                      (get-signal a1)
                      (get-signal a2))])
      (after-delay agenda or-gate-delay (λ () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (make-wire)
  (define (call-each procedures)
    (if (null? procedures)
        'done
        (begin ((car procedures))
               (call-each (cdr procedures)))))
  
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

; Agenda

(define (make-agenda)
  (define agenda (cons 0 '()))
  (define (set-current-time! time) (set-car! agenda time))
  (define (segments) (cdr agenda))
  (define (set-segments! segments) (set-cdr! agenda segments))
  (define (first-segment) (car (segments)))
  (define (rest-segments) (cdr (segments)))
  (define (empty-agenda?) (null? (segments)))
  
  (define (current-time) (car agenda))
  
  (define (after-delay delay action)
    (add-to-agenda! (+ delay (current-time)) action))

  (define (add-to-agenda! time action)
    (define (belongs-before? segments)
      (if (null? segments) #t (< time (car (car segments)))))

    (define (make-time-segment)
      (let ([q (make-queue)])
        (enqueue! q action)
        (cons time q)))

    (define (add-to-segments! segments)
      (if (= time (car (car segments)))
          (enqueue! (cdr (car segments)) action)
          (let ([rest (cdr segments)])
            (if (belongs-before? rest)
                (let ([new-seg (make-time-segment)])
                  (set-cdr! segments (cons new-seg rest)))
                (add-to-segments! rest)))))
              
    (let ([segments (segments)])
      (if (belongs-before? segments)
          (set-segments! (cons (make-time-segment) segments))
          (add-to-segments! segments))))

  (define (propagate)
    (if (empty-agenda?)
        'done
        (let ([first-item (first-agenda-item)])
          (first-item)
          (remove-first-agenda-item!)
          (propagate))))
  
  (define (first-agenda-item)
    (if (empty-agenda?)
        (error "Agenda is empty: FIRST-AGENDA-ITEM")
        (let ([first-seg (first-segment)])
          (set-current-time! (car first-seg))
          (peek (cdr first-seg)))))

  (define (remove-first-agenda-item!)
    (let ([q (cdr (first-segment))])
      (dequeue! q)
      (if (empty-queue? q)
          (set-segments! (rest-segments))
          #f)))
  
  (define (dispatch m)
    (cond
      [(eq? m 'current-time) current-time]
      [(eq? m 'after-delay) after-delay]
      [(eq? m 'propagate) propagate]
      [else (error "Unknown operation: AGENDA" m)]))
  dispatch)

(define (after-delay agenda delay action) ((agenda 'after-delay) delay action))
(define (current-time agenda) ((agenda 'current-time)))
(define (propagate agenda) ((agenda 'propagate)))

; Test
(define (half-adder a b s c agenda)
  (let ([d (make-wire)]
        [e (make-wire)])
    (or-gate a b d agenda)
    (and-gate a b c agenda)
    (inverter c e agenda)
    (and-gate d e s agenda)
    'ok))

(define (probe name wire agenda)
  (add-action!
   wire
   (λ ()
     (display name) (display " ") (display (current-time agenda)) (display " New-value = ") (display (get-signal wire))
     (newline))))

;(define agenda (make-agenda))
;(define input-1 (make-wire))
;(define input-2 (make-wire))
;(define sum (make-wire))
;(define carry (make-wire))
;(probe "sum" sum agenda)
;(probe "carry" carry agenda)
;(half-adder input-1 input-2 sum carry agenda)
;(set-signal! input-1 1)
;(propagate agenda)
;(set-signal! input-2 1)
;(propagate agenda)