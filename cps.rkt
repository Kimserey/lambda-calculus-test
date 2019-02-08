#lang racket
(require racket/trace)

(define fact
  (λ (n)
    (cond
      [(zero? n) 1]
      [else (* (fact (- n 1)) n)])))

(trace fact)
(fact 5)

(define fact-aps
  (λ (a n)
    (cond
      [(zero? n) a]
      [else (fact-aps (* n a) (- n 1))])))

(trace fact-aps)
(fact-aps 1 5)

(define fact-cps
  (λ (n k)
    (cond
      [(zero? n) (k 1)]
      [else
        (fact-cps (- n 1)
                  (λ (v) (k (* v n))))])))

(trace fact-cps)
(fact-cps 5 (λ (n) n))