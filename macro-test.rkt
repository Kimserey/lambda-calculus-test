#lang racket

; Define a syntax
; (syntax (lambda (k) k))
; #'(+ 1 2)
; #'"hello"

; quote
; '(lambda (k) k)
; '(+ 1 2)
; '"hello"

; syntax transformer

(define-syntax foo
  (lambda (stx)
    #'(displayln "I am foo")))
; (foo)

(define-syntax (foo-two stx) #'"I am foo 2")
; (foo-two)

; Composition of a syntax is the source location and the quoted S-expression
(define-syntax (show-me stx)
  #'"hello")
;(show-me '(+ 1 12))

(define stx #'(+ 1 (+ 2 (+ 3 3))))
; (syntax-source stx)
; (syntax-line stx)
; (syntax-column stx)

; return datum from syntax (quote)
; (syntax->datum stx)

; goes one level down and return list of syntax objects
; (syntax-e stx)
;(syntax->list stx)

(define-syntax (reverse-me stx)
  ; '(reverse-me "i" "am" "kim" values)
  (println (syntax->datum stx))
  ; '("i" "am" "kim" values)
  (println (cdr (syntax->datum stx)))
  ; '(values "kim" "am" "i")
  (println (reverse (cdr (syntax->datum stx))))
  ; (values "kim" "am" "i")
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))

;(reverse-me "I" "am" "Kim" values)

(define (our-if condition true-expr false-expr)
  (cond
    [condition true-expr]
    [else false-expr]))

(define (display-and-return x)
  (displayln x)
  x)

; The two display procedures are evaluated before our-if is called
; (our-if #t
;        (display-and-return "true")
;        (display-and-return "false"))

(define-syntax (our-if-v2 stx)
  ; '(our-if-v2 #f (display-and-return "true") (display-and-return "false"))
  (define xs (syntax->list stx))
  ; unquote form escape the quote and the result of the expression will take place of the unquote in the quasiquote result.
  ; , is an abbreviation for unquote.
  ; ` is an abbreviation for quasiquote.
  ; #' is an abbreviation for syntax.
  ; ' is an abbreviation for quote.
  (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                            [else ,(cadddr xs)])))


; datum->syntax is a procedure going from a datum (a datum is a quoted s-expression) to a syntax (a datum with source location).
; (require (for-syntax _)) requires a function for compile time - at compile time only racket/base is automatically required..
(require (for-syntax racket/match))
(define-syntax (our-if-using-match stx)
  (match (syntax->list stx)
    [(list name condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))

; begin-for-syntax is used to define procedures to be used within define-syntax (at compile time)
(begin-for-syntax
  (define (proc-for-my-syntax)
    #'(displayln "hello")))
(define-syntax (test stx)
  (proc-for-my-syntax))

; define-for-syntax is an abbreviation of (being-for-syntax (define _ _))
(define-for-syntax (proc-for-my-syntax-v2)
  #'(displayln "hello"))
(define-syntax (test-v2 stx)
  (proc-for-my-syntax-v2))