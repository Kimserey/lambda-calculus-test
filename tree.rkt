#lang racket

; Consider the follwoing Baskets of apples problem.
;

(define x 10)

(require pict pict/tree-layout)
(define (draw tree)
  (define (viz tree)
    (cond
      ((null? tree) #f)
      ((not (pair? tree))
       (tree-layout #:pict (cc-superimpose
                            (disk 50 #:color "gray")
                            (text (symbol->string tree)))))
      ((not (pair? (car tree)))
       (apply tree-layout (map viz (cdr tree))
              #:pict (cc-superimpose
                      (disk 50 #:color "white")
                      (text (symbol->string (car tree))))))))
  (if (null? tree)
      #f
      (naive-layered (viz tree))))

(define tree '(limit/80 (limit/50 basket/20 basket/10) (limit/50 basket/20 basket/10)))
(draw tree)