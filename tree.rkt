#lang racket

(require pict pict/tree-layout)

(define (circle t col children)
  (tree-layout children #:pict (cc-superimpose (disk 50 #:color col) (text t))))

(define (rectangle t col)
  (tree-layout #:pict (cc-superimpose (filled-rectangle 100 50 #:color col) (text t))))

(define tree (tree-layout ))

(define (build-layout tree)
  (cond
    [(not (pair? tree))
     (tree-layout #:pict (cc-superimpose
                          (filled-rectangle 100 50 #:color "gray")
                          (text (symbol->string tree))))]
    [else
     (apply tree-layout
            #:pict (cc-superimpose
                    (disk 70 #:color "white")
                    (text (symbol->string (car tree))))
            (map build-layout (cdr tree)))]))

(define (draw tree) (naive-layered (build-layout tree)))

(draw '(limit/50
        (limit/100 basket/200 basket/100)
        (limit/50 basket/100 basket/10)))