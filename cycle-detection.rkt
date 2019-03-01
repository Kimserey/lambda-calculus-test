#lang racket

(define nodes
  (list
   (list 1 2)
   (list 2 3)
   (list 3 4)
   (list 4 2)))

(define (find-meeting next p1 p2)
  (let ([slow (next p1)]
        [fast (next (next p2))])
    (cond
      [(= slow fast) slow]
      [else (find-meeting next slow fast)])))

(define (get-linked-node parent)
  (define (find nodes parent)
    (let* ([node (car nodes)]
           [p (car node)]
           [c (cadr node)])
      (if (= parent p) c (find (cdr nodes) parent))))
  (find nodes parent))
      