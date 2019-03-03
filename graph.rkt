#lang racket

(require graph)
(define g (unweighted-graph/directed '((1 2) (2 3) (3 4) (4 5) (5 3))))

(graphviz g)