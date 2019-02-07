#lang racket
(require racket/trace)

(define fact
  (λ (n)
    (cond
      [(zero? n) 1]
      [else (* (fact (- n 1)) n)])))

(trace fact)
(fact 5)

(define fact-tc
  (λ (result n)
    (cond
      [(zero? n) result]
      [else (fact-tc (* n result) (- n 1))])))

(trace fact-tc)
(fact-tc 1 5)

(define fact-cps
  