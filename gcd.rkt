#lang racket

(define (gcd-test a b)
  (let ([rest (modulo a b)])
    (if (zero? rest) b (gcd-test b rest))))