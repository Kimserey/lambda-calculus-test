#lang racket

(define nodes
  (list
   (cons 1 2)
   (cons 2 3)
   (cons 3 4)
   (cons 4 2)
   (cons 5 6)))

; Finds the child given a parent
(define (next parent)
  (define (find nodes parent)
    (let* ([node (car nodes)]
           [p (car node)]
           [c (cdr node)])
      (if (= parent p) c (find (cdr nodes) parent))))
  (find nodes parent))

(define (next/next x) (next (next x)))

; Traverses a list at the speed specified
(define (traverse slow fast p1 p2 steps)
  (let ([p1 (slow p1)]
        [p2 (fast p2)])
    (cond
      [(= p1 p2) (cons p1 steps)]
      [else (traverse slow fast p1 p2 (+ 1 steps))])))

; Extracts result from traverse
(define (traverse/result slow fast p1 p2)
  (car (traverse slow fast p1 p2 0)))

; Extracts the count of iteration from traverse 
(define (traverse/count slow fast p1 p2)
  (cdr (traverse slow fast p1 p2 0)))

; Finds the meeting point, cycle root, and length of cycle
(define (cycle-detection init)
  (define meeting
    (traverse/result next next/next init init))
  (define root
    (traverse/result next next init meeting))
  (define length
    (traverse/count next identity root root))
  (list meeting root length))