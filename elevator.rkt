#lang racket

(define (contains x xs)
  (cond [(null? xs) #f]
        [(eq? x (car xs)) #t]
        [else (contains x (cdr xs))]))

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

(define (continue? direction level requests) #t)

(define (get-requests) '(3 5 6 9))

(define (move-elevator direction level)
  (let ([current-level (update-level direction level)]
        [requests (get-requests)])
    (if (open? current-level requests)
        (displayln "open")
        (displayln "skip"))
    (cond
      [(null? requests) 'done]
      [(continue? direction current-level requests) (move-elevator direction current-level)]
      [else (move-elevator (opposite direction) current-level)])))