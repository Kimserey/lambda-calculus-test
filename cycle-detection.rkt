#lang racket

; Floyd's Tortoise and Hare

(define nodes
  (list
   (cons 1 2)
   (cons 2 3)
   (cons 3 4)
   (cons 4 5)
   (cons 5 2)))

; Finds the child given a parent
(define (next parent)
  (define (find nodes parent)
    (match nodes
      [(list (cons p c) rest ...) #:when (= parent p) c]
      [_ (find (cdr nodes) parent)]))
  (find nodes parent))

(define (next-next x) (next (next x)))

; Traverses a list at the speed specified
(define (meet slow fast p1 p2 steps)
  (let ([p1 (slow p1)]
        [p2 (fast p2)]
        [steps (+ 1 steps)])
    (cond
      [(= p1 p2) (cons p1 steps)]
      [else (meet slow fast p1 p2 steps)])))

; Extracts result from traverse
(define (meet/result slow fast p1 p2)
  (car (meet slow fast p1 p2 0)))

; Extracts the count of iteration from traverse 
(define (meet/count slow fast p1 p2)
  (cdr (meet slow fast p1 p2 0)))

; Finds the meeting point, cycle root, and length of cycle
(define (find-cycle init)
  
  ; Step 1: Traverses linked list with tortoise i and hare 2i
  ; to find meeting point.
  (define cycle-meet
    (meet/result next next-next init init))

  ; Step 2: Move hare back to beginning and traverse
  ; to find cycle root at same speed i.
  (define cycle-root
    (meet/result next next cycle-meet init))

  ; Step 3: Keep tortoise in place and move hare
  ; on the cycle to execute one full circle to find
  ; the length.
  (define cycle-length
    (meet/count identity next cycle-root cycle-root))
  
  (list cycle-meet cycle-root cycle-length))