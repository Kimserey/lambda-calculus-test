#lang racket

(require pict pict/tree-layout)

(define (tree-layout/titled node title subtitle child)
    (apply tree-layout #:pict (cc-superimpose node (vc-append (text title) (text subtitle))) child))

(define (c/o title subtitle [child '()])
  (tree-layout/titled (disk 70 #:color "orange") title subtitle child))

(define (c/w title subtitle [child '()])
  (tree-layout/titled (disk 70 #:color "white") title subtitle child))

(define (r/o title subtitle [child '()])
  (tree-layout/titled (filled-rectangle 150 50 #:color "orange") title subtitle child))

(define (r/g title subtitle [child '()])
  (tree-layout/titled (filled-rectangle 150 50 #:color "gray") title subtitle child))

(naive-layered
  (c/o "limit 1" "0"
                (list
                 (c/o "limit 2" "0"
                      (list
                       (r/o "basket 1" "limit: 0 | apples: 1")
                       (r/g "basket 2" "limit: 0 | apples: 1")))
                 (c/w "limit 3" "0"
                      (list
                       (r/g "basket 3" "limit: 0 | apples: 1")
                       (r/g "basket 4" "limit: 0 | apples: 1"))))))

(naive-layered
  (c/o "limit 1" "0"
                (list
                 (c/w "limit 2" "0"
                      (list
                       (r/g "basket 1" "limit: 0 | apples: 1")
                       (r/g "basket 2" "limit: 0 | apples: 1")))
                 (c/o "limit 3" "0"
                      (list
                       (r/g "basket 3" "limit: 0 | apples: 1")
                       (r/o "basket 4" "limit: 0 | apples: 1"))))))