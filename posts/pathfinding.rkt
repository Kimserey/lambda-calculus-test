#lang racket

; Prelude
(define (filter proc xs)
  (cond [(null? xs) '()]
        [(proc (car xs)) (cons (car xs) (filter proc (cdr xs)))]
        [else (filter proc (cdr xs))]))

(define (contains x xs)
  (cond [(null? xs) #f]
        [(eq? x (car xs)) #t]
        [else (contains x (cdr xs))]))

; Selectors
(define (node-letter node) (car node))
(define (node-value node) (cadr node))
(define (node-path node) (caddr node))
(define (link-from link) (car link))
(define (link-to link) (cadr link))
(define (link-distance link) (caddr link))

; A node is defined by a letter, a travelled distance, and a sequence of nodes.
(define (smallest xs)
  (define (loop result xs)
    (cond [(null? xs) result]
          [(< (node-value (car xs)) (node-value result)) (loop (car xs) (cdr xs))]
          [else (loop result (cdr xs))]))
  (if (null? xs) 'none (loop (car xs) (cdr xs))))

; Appends a node to a sequence if it does not exist in the sequence,
; else replace the node if the distance is lower.
(define (node-append x xs)
  (cond [(null? xs) (cons x '())]
        [(eq? (car x) (caar xs))
         (if (< (cadr x) (cadar xs)) (cons x (cdr xs)) xs)]
        [else (cons (car xs) (node-append x (cdr xs)))]))

; Adjoins a list of node by recursively appending each element.
(define (node-adjoin ls xs)
  (cond [(null? ls) xs]
        [else (node-append (car ls) (node-adjoin (cdr ls) xs))]))

; Excludes a node from a sequence
(define (node-exclude x xs)
  (cond [(null? xs) '()]
        [(eq? (node-letter x) (node-letter (car xs))) (cdr xs)]
        [else (cons (car xs) (node-exclude x (cdr xs)))]))

; Gets the adjacents nodes of a particular node given a list of links.
(define (adjacents from links)
  (filter (λ (link) (eq? (node-letter from) (link-from link))) links))

; Gets the links which egde is excluded from the list provided.
(define (excluded-from xs links)
  (filter (λ (link) (not (contains (link-to link) xs))) links))

; Gets the adjacent links which are excluded from the list provided. 
(define (adjacents-excluded-from xs from links)
  (excluded-from xs (adjacents from links)))

(define (make-node name distance path)
  (list name distance path))

; Gets nodes out of links excluded from the closed nodes.
(define (node-expand from closed links)
  (map (λ (link) (make-node (link-to link)
                            (+ (node-value from) (link-distance link))
                            (cons (link-from link) (node-path from))))
       (adjacents-excluded-from closed from links)))

; 1. Expands node by getting all adjacent nodes not included in closed nodes,
; 2. Adjoins adjacent nodes with open nodes, replacing with shortest paths,
; 3. Take the shortest path in open nodes and recurse on it,
; 4. Stop if shortest path in open node is the destination.
(define (dijkstra start stop links)
  (define (destination? node)
    (eq? (node-letter node) stop))
  
  (define (find start opened closed)
    (let* ([adjacents (node-expand start closed links)]
           [opened (node-adjoin adjacents (node-exclude start opened))]
           [top (smallest opened)])
      
      (if (destination? top)
          top
          (find top opened (cons (node-letter start) closed)))))

  (find (make-node start 0 '()) '() '()))

; Run
(define links
  (list
   (list 'S 'A 7)
   (list 'A 'G 10)
   (list 'S 'C 2)
   (list 'C 'G 3)
   (list 'S 'D 1)
   (list 'D 'E 2)
   (list 'D 'F 3)
   (list 'E 'G 5)
   (list 'F 'G 7)))

(dijkstra 'S 'G links)