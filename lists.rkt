#lang racket

(define (triple? xs)
  (and (pair? xs) (pair? (cdr xs)) (pair? (cddr xs))))

(define (append x xs)
  (if (null? x)
      xs
      (cons (car x) (append (cdr x) xs))))

(define (find xs)
  (define (find res1 res2 xs)
    (if (null? xs)
        (list res1 res2)
        (if (triple? xs)
            (let ([first  (car xs)]
                  [second (cadr xs)]
                  [third  (caddr xs)])
              (if (and (equal? first second) (equal? second third))
                  (find (cons first res1) res2 (cdddr xs))
                  (find res1 (cons first res2) (cdr xs))))
            (list res1 (append xs res2)))))
  (find '() '() xs))