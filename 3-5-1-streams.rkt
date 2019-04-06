#lang racket

; Iterative calculation

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0)) 
  (define (find-divisor n test-divisor)
    (cond [(> (* test-divisor test-divisor) n) n]
          [(divides? test-divisor n) test-divisor]
          [else (find-divisor n (+ test-divisor 1))]))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-primes a b)
  (define (iter count acc)
    (cond [(> count b) acc]
          [(prime? count) (iter (+ count 1) (+ count acc))]
          [else (iter (+ count 1) acc)]))
  (iter a 0))

; Sequence operations calculation

(define (filter predicate sequence)
  (cond [(null? sequence) '()]
        [(predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (sum-primes-two a b)
  (accumulate + 0 (filter prime? (enumerate-interval a b))))