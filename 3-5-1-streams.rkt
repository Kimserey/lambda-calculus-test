#lang racket

; Iterative calculation

(define (divides? a b) (= (remainder b a) 0)) 

(define (smallest-divisor n)
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
; Even though composable, problematic due to the whole sequence
; needing to be computed prior the next operation start.

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

; Streams

(define the-empty-stream 'empty-stream)

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (define (force x) (x))
  (force (cdr stream)))

(define (memo-proc proc)
  (let ([already-run? false]
        [result false])
    (λ ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            ;(display "Memo: Execute ")
            ;(display result)
            ;(newline)
            (set! already-run? true)
            result)
          result))))

(define (stream-null? s)
  (eq? s the-empty-stream))

; Generator will delay the evaluation of cdr
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons low (memo-proc (λ () (stream-enumerate-interval (+ 1 low) high))))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons
       (proc (stream-car s))
       (memo-proc (λ () (stream-map proc (stream-cdr s)))))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc
                         (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each displayln s))

(define (stream-filter pred stream)
  (cond [(stream-null? stream) the-empty-stream]
        [(pred (stream-car stream))
         (cons (stream-car stream)
               (memo-proc (λ () (stream-filter pred (stream-cdr stream)))))]
        [else (stream-filter pred (stream-cdr stream))]))

; Infinite stream

(define (integers-starting-from n)
  (cons n (λ () (integers-starting-from (+ n 1)))))

(define integers (integers-starting-from 1))

(define no-sevens
  (stream-filter (λ (x) (not (divides? 7 x))) integers))

(define (fibgen a b)
  (cons a (λ () (fibgen b (+ a b)))))

(define fibs (fibgen 0 1))

; Sieve of Eratosthenes
; Recursively curates the stream by removing divisibles
; of each primes.
; At each curation, the prime is cons'ed with the rest,
; resulting stream not longer contains divisable value of the prime returned.
;
; Stream is infinite, and the process is also infinite due to the sieve containing the sieve.
(define (sieve stream)
  (cons
   (stream-car stream)
   (λ ()
     (sieve
      (stream-filter
       (λ (x) (not (divides? (stream-car stream) x)))
       (stream-cdr stream))))))

(define primes (sieve (integers-starting-from 2)))