#lang racket

(define (contains x xs)
  (cond [(null? xs) #f]
        [(eq? x (car xs)) #t]
        [else (contains x (cdr xs))]))

(define (filter predicate xs)
  (define (filter xs res)
    (cond [(null? xs) res]
          [(predicate (car xs)) (filter (cdr xs) (cons (car xs) res))]
          [else (filter (cdr xs) res)]))
  (filter xs '()))

(define (any predicate xs)
  (cond [(null? xs) #f]
        [(predicate (car xs)) #t]
        [else (any predicate (cdr xs))]))

(define (down? direction) (eq? 'down direction))
(define (up? direction) (eq? 'up direction))

(define (opposite direction)
  (cond [(up? direction) 'down]
        [(down? direction) 'up]))

(define (update-level direction level)
  (cond [(up? direction) (+ level 1)]
        [(down? direction) (- level 1)]))

(define (open? level requests) (contains level requests))

(define (continue? direction level requests)
  (if (up? direction)
      (any (λ (l) (< level l)) requests)
      (any (λ (l) (> level l)) requests)))

(define requests '(3 5 6 9 4))
(define (remove-request level) (set! requests (filter (λ (l) (not (eq? l level))) requests)))
(define (get-requests) requests)
(define (simulate-new-request)
  (let ([new-req (read-line (current-input-port) 'any)])
    (if (non-empty-string? new-req)
        (set! requests (cons (string->number new-req) requests))
        0)))


(define (move-elevator direction level)
  (let ([current-level (update-level direction level)]
        [requests (get-requests)])
    (displayln (format "+Direction ~a" direction))
    (if (open? current-level requests)
        (displayln (format "+Open ~a" current-level))
        (displayln (format "+Skip ~a" current-level)))
    (remove-request current-level)
    (let ([requests (get-requests)])
      (displayln (format "++Requested: ~a" requests))
      (simulate-new-request)
      (cond
        [(null? requests) 'idle]
        [(continue? direction current-level requests) (move-elevator direction current-level)]
        [else (move-elevator (opposite direction) current-level)]))))