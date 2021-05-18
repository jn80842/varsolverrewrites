#lang racket

(require parser-tools/yacc)

(require "lexer.rkt")
(require "halide-lang.rkt")

(provide halide->rktlang
         halide->renamevars
         halide->countops)

(define halide->rktlang-parser
  (parser

   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))

   (precs (left OR AND)
          (right EQ)
          (left < > GE LE)
          (left - +)
          (left * / %)
          (left NEG)
          (left !))

   (grammar

    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])
    
    (exp [(NUM) $1]
         [(VAR) $1]
         [(TRUE) "true"]
         [(FALSE) "false"]
         [(UINT1) "true"]
         [(UINT0) "false"]
         [(exp EQ exp) (format "(eq? ~a ~a)" $1 $3)]
         [(MAX OP exp COMMA exp CP) (format "(max ~a ~a)" $3 $5)]
         [(MIN OP exp COMMA exp CP) (format "(min ~a ~a)" $3 $5)]
         [(SELECT OP exp COMMA exp COMMA exp CP) (format "(if ~a ~a ~a)" $3 $5 $7)]
         [(exp AND exp) (format "(hld-and ~a ~a)" $1 $3)]
         [(exp OR exp) (format "(hld-or ~a ~a)" $1 $3)]
         [(exp + exp) (format "(hld-add ~a ~a)"$1 $3)]
         [(exp - exp) (format "(hld-sub ~a ~a)" $1 $3)]
         [(exp * exp) (format "(hld-mul ~a ~a)" $1 $3)]
         [(exp / exp) (format "(hld-div ~a ~a)" $1 $3)]
         [(exp < exp) (format "(hld-lt ~a ~a)" $1 $3)]
         [(exp > exp) (format "(hld-gt ~a ~a)" $1 $3)]
         [(exp % exp) (format "(hld-mod ~a ~a)" $1 $3)]
         [(exp GE exp) (format "(hld-ge ~a ~a)" $1 $3)]
         [(exp LE exp) (format "(hld-le ~a ~a)" $1 $3)]
         [(! OP exp CP) (format "(hld-not ~a)" $3)]
         [(OP exp CP) $2]
         [(LII exp) $2]))))

(define (halide->renamevars-parser varmap)
  (parser

   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))

   (precs (left OR AND)
          (right EQ)
          (left < > GE LE)
          (left - +)
          (left * / %)
          (left NEG)
          (left !))
   
   (grammar
    
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])

    (exp [(NUM) $1]
         [(VAR) (hash-ref varmap (symbol->string $1))]
         [(TRUE) "true"]
         [(FALSE) "false"]
         [(UINT1) "true"]
         [(UINT0) "false"]
         [(exp EQ exp) (format "(~a == ~a)" $1 $3)]
         [(MAX OP exp COMMA exp CP) (format "max(~a, ~a)" $3 $5)]
         [(MIN OP exp COMMA exp CP) (format "min(~a, ~a)" $3 $5)]
         [(SELECT OP exp COMMA exp COMMA exp CP) (format "select(~a, ~a, ~a)" $3 $5 $7)]
         [(exp AND exp) (format "(~a && ~a)" $1 $3)]
         [(exp OR exp) (format "(~a || ~a)" $1 $3)]
         [(exp + exp) (format "(~a + ~a)"$1 $3)]
         [(exp - exp) (format "(~a - ~a)" $1 $3)]
         [(exp * exp) (format "(~a * ~a)" $1 $3)]
         [(exp / exp) (format "(~a/~a)" $1 $3)]
         [(exp < exp) (format "(~a < ~a)" $1 $3)]
         [(exp > exp) (format "(~a > ~a)" $1 $3)]
         [(exp % exp) (format "(~a%~a)" $1 $3)]
         [(exp GE exp) (format "(~a >= ~a)" $1 $3)]
         [(exp LE exp) (format "(~a <= ~a)" $1 $3)]
         [(! OP exp CP) (format "!(~a)" $3)]
         [(OP exp CP) $2]
         [(LII exp) $2]))))

(define halide->countops-parser
  (parser

   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))

   (precs (left OR AND)
          (right EQ)
          (left < > GE LE)
          (left - +)
          (left * / %)
          (left NEG)
          (left !))

   (grammar

    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])

    (exp [(NUM) 0]
         [(VAR) 0]
         [(TRUE) 0]
         [(FALSE) 0]
         [(UINT1) 0]
         [(UINT0) 0]
         [(exp EQ exp) (+ 1 $1 $3)]
         [(MAX OP exp COMMA exp CP) (+ 1 $3 $5)]
         [(MIN OP exp COMMA exp CP) (+ 1 $3 $5)]
         [(SELECT OP exp COMMA exp COMMA exp CP) (+ 1 $3 $5 $7)]
         [(exp AND exp) (+ 1 $1 $3)]
         [(exp OR exp) (+ 1 $1 $3)]
         [(exp + exp) (+ 1 $1 $3)]
         [(exp - exp) (+ 1 $1 $3)]
         [(exp * exp) (+ 1 $1 $3)]
         [(exp / exp) (+ 1 $1 $3)]
         [(exp < exp) (+ 1 $1 $3)]
         [(exp > exp) (+ 1 $1 $3)]
         [(exp % exp) (+ 1 $1 $3)]
         [(exp GE exp) (+ 1 $1 $3)]
         [(exp LE exp) (+ 1 $1 $3)]
         [(! OP exp CP) (+ 1 $3)]
         [(OP exp CP) $2]
         [(LII exp) $2]))))

(define (halide->rktlang s)
  (let ([expr (evaluate-halide-parser halide->rktlang-parser s)])
    (cond [(string-contains? expr "w") (string-append "(λ (x y z w) " expr ")")]
          [(string-contains? expr "z") (string-append "(λ (x y z) " expr ")")]
          [(string-contains? expr "y") (string-append "(λ (x y) " expr ")")]
          [else (string-append "(λ (x) " expr " ")])))

(define (halide->renamevars s varmap)
  (evaluate-halide-parser (halide->renamevars-parser varmap) s))

(define (halide->countops s)
  (evaluate-halide-parser halide->countops-parser s))
