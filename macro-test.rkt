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

; Fear of the macro - Transform

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

; Equivalent of our-if-using-match
(define-syntax (our-if-using-syntax-case stx)
  (syntax-case stx ()
    [(_ condition true-expr false-expr)
     #'(cond [condition true-expr]
             [else false-expr])]))

; With define-syntax-rule the return type isn't a syntax
(define-syntax-rule (our-if-using-syntax-rule condition true-expr false-expr)
  (cond [condition true-expr]
        [else false-expr]))

;(define-syntax (hyphen-define/ok1 stx)
;  (syntax-case stx ()
;    [(_ a b (args ...) body0 body ...)
;     (syntax-case (datum->syntax #'a (string->symbol (format "~a-~a" (syntax->datum #'a) (syntax->datum #'b))))
;       ()
;       [name #'(define (name args ...)
;                 body0 body ...)])]))

;(define-syntax (hyphen-define/ok2 stx)
;  (syntax-case stx ()
;    [(_ a b (args ...) body0 body ...)
;     (with-syntax ([name (datum->syntax #'a (string->symbol (format "~a-~a" (syntax->datum #'a) (syntax->datum #'b))))])
;        #'(define (name args ...)
;                 body0 body ...))]))

; Fear of the macro - Pattern matching

(require (for-syntax racket/syntax))
(define-syntax (hyphen-define/ok3 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name (format-id #'a "~a-~a" #'a #'b)])
        #'(define (name args ...)
                 body0 body ...))]))

(require (for-syntax racket/string racket/syntax))
(define-syntax (hyphen-define* stx)
  (syntax-case stx ()
    [(_ (names ...) (args ...) body0 body ...)
     (let ([name-stxs (syntax->list #'(names ...))])
       (with-syntax ([name (datum->syntax (car name-stxs)
                                          (string->symbol
                                           (string-join (for/list ([name-stx name-stxs])
                                                          (symbol->string (syntax-e name-stx))) "-")))])
         #`(define (name args ...)
             body0 body ...)))]))

; define a struct with accessor via (foo-a val)
(define-syntax (our-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     ; Fender expression
     (for-each (lambda (x)
                 (unless (identifier? x)
                   (raise-syntax-error #f "not an identifier" stx x)))
               (cons #'id (syntax->list #'(fields ...))))
     (with-syntax ([pred-id (format-id #'id "~a?" #'id)])
       #`(begin
           ; Define a constructor
           (define (id fields ...)
             (apply vector (cons 'id (list fields ...))))
           ; Define a predicate
           (define (pred-id v)
             (and (vector? v)
                  (eq? (vector-ref v 0) 'id)))
           ; Define an accessor for each field
           #,@(for/list ([x (syntax->list #'(fields ...))]
                         [n (in-naturals 1)])
                (with-syntax ([acc-id (format-id #'id "~a-~a" #'id x)]
                              [ix n])
                  #`(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id))
                      (vector-ref v ix))))))]))

(define-syntax (hash.refs stx)
  (syntax-case stx ()
    [(_)
     (raise-syntax-error #f "Expected hash.key0[.key1 ...] [default]" stx #'chain)]
    [(_ chain)
     #'(hash.refs chain #f)]
    [(_ chain default)
     (unless (identifier? #'chain)
         (raise-syntax-error #f "Expected hash.key0[.key1 ...] [default]" stx #'chain))
     (let* ([chain-str (symbol->string (syntax->datum #'chain))]
            [ids (for/list ([str (in-list (regexp-split #rx"\\." chain-str))])
                   (format-id #'chain "~a" str ))])
       (unless (and (>= (length ids) 2)
                    (not (eq? (syntax-e (cadr ids)) '||)))
         (raise-syntax-error #f "Expected hash.key" stx #'chain))
       (with-syntax ([hash-table (car ids)]
                     [keys (cdr ids)])
         #'(hash-refs hash-table 'keys default)))]))