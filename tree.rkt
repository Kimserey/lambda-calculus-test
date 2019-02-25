#lang racket

(require pict pict/tree-layout)

(define (tree-layout/titled node title subtitle child)
    (apply
     tree-layout
     #:pict (cc-superimpose node (vc-append (text title) (text subtitle)))
     (map (λ (node) (tree-edge #:edge-width 3 #:edge-color "black" node)) child)))

(define (c/o title subtitle [child '()])
  (tree-layout/titled (disk 70 #:border-width 3 #:color "orange") title subtitle child))

(define (c/w title subtitle [child '()])
  (tree-layout/titled (disk 70 #:border-width 3 #:color "white") title subtitle child))

(define (r/o title subtitle [child '()])
  (tree-layout/titled (filled-rectangle 150 50 #:border-width 3 #:color "orange") title subtitle child))

(define (r/g title subtitle [child '()])
  (tree-layout/titled (filled-rectangle 150 50 #:border-width 3 #:color "gray") title subtitle child))

(define pic 
  (naive-layered
   (c/o "limit 1" "0"
        (list
         (c/w "limit 2" "10"
              (list
               (r/g "basket A" "slots: 10 | apples: 10")
               (r/g "basket B" "slots: 10 | apples: 0")))
         (c/o "limit 3" "15"
              (list
               (r/g "basket C" "slots: 5 | apples: 5")
               (r/o "basket D" "implicit slots: 0 | apples: 0")))))))

(send (pict->bitmap pic) save-file "img\\example.png" 'png)
