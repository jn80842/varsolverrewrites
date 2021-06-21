#lang racket

(require parser-tools/yacc)

(require "lexer.rkt")
(require "halide-lang.rkt")
(require "trat/termIR.rkt")

(provide halide->rktlang
         halide->renamevars
         halide->countops
         halide-expr-in-solved-form?
         halide->termIR)

(define halide->termIR-parser
  (parser

   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))

   (precs (left OR AND)
          (right EQ NEQ)
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
    (boolvar [(UINT VAR) $2]
             [(UINT VAR) $2])

    (exp [(NUM) $1]
         [(VAR) (symbol->string $1)]
         [(TVAR) (symbol->string $1)]
         [(NTVAR) (symbol->string $1)]
         [(boolvar) (symbol->string $1)]
         [(TRUE) "true"] ;; need to add a boolean type
         [(FALSE) "false"]
         [(UINT1) "true"]
         [(UINT0) "false"]
         [(exp EQ exp) (sigma-term '== (list $1 $3))]
         [(exp NEQ exp) (sigma-term '!= (list $1 $3))]
         [(MAX OP exp COMMA exp CP) (sigma-term 'max (list $3 $5))]
         [(MIN OP exp COMMA exp CP) (sigma-term 'min (list $3 $5))]
         [(SELECT OP exp COMMA exp COMMA exp CP) (sigma-term 'select (list $3 $5 $7))]
         [(exp AND exp) (sigma-term '&& (list $1 $3))]
         [(exp OR exp) (sigma-term 'or (list $1 $3))]
         [(exp + exp) (sigma-term '+ (list $1 $3))]
         [(exp - exp) (sigma-term '- (list $1 $3))]
         [(exp * exp) (sigma-term '* (list $1 $3))]
         [(exp / exp) (sigma-term '/ (list $1 $3))]
         [(exp < exp) (sigma-term '< (list $1 $3))]
         [(exp > exp) (sigma-term '> (list $1 $3))]
         [(exp % exp) (sigma-term '% (list $1 $3))]
         [(exp GE exp) (sigma-term '>= (list $1 $3))]
         [(exp LE exp) (sigma-term '<= (list $1 $3))]
         [(! OP exp CP) (sigma-term '! (list $3))]
         [(OP exp CP) $2]
         [(LII exp) $2]
         [(LIKELY OP exp CP) $3]))))

(define halide->solvedform-parser
  (parser
   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))

   (precs (left OR AND)
          (right EQ NEQ)
          (left < > GE LE)
          (left - +)
          (left * / %)
          (left NEG)
          (left !))

   (grammar
    (start [() #f]
           [(targetvar binop nt-exp) #t]
           [(OP targetvar binop nt-exp CP) #t]
           [(binop targetvar nt-exp) #t]
           [(OP binop targetvar nt-exp CP) #t]
           [(nt-exp) #t])
    (targetvar [(TVAR) #t])
    (binop [(EQ) #t]
           [(MAX) #t]
           [(MIN) #t]
           [(AND) #t]
           [(OR) #t]
           [(+) #t]
           [(-) #t]
           [(*) #t]
           [(/) #t]
           [(%) #t]
           [(<) #t]
           [(>) #t]
           [(GE) #t]
           [(LE) #t])
    (nt-exp [(NUM) $1]
         [(NTVAR) $1]
         [(TRUE) "true"]
         [(FALSE) "false"]
         [(UINT1) "true"]
         [(UINT0) "false"]
         [(nt-exp EQ nt-exp) (format "(eq? ~a ~a)" $1 $3)] ;; we don't use the halide-lang methods so we don't have to type args
         [(nt-exp NEQ nt-exp) (format "(not (eq? ~a ~a))" $1 $3)]
         [(MAX OP nt-exp COMMA nt-exp CP) (format "(max ~a ~a)" $3 $5)]
         [(MIN OP nt-exp COMMA nt-exp CP) (format "(min ~a ~a)" $3 $5)]
         [(SELECT OP nt-exp COMMA nt-exp COMMA nt-exp CP) (format "(if ~a ~a ~a)" $3 $5 $7)]
         [(nt-exp AND nt-exp) (format "(hld-and ~a ~a)" $1 $3)]
         [(nt-exp OR nt-exp) (format "(hld-or ~a ~a)" $1 $3)]
         [(nt-exp + nt-exp) (format "(hld-add ~a ~a)"$1 $3)]
         [(nt-exp - nt-exp) (format "(hld-sub ~a ~a)" $1 $3)]
         [(nt-exp * nt-exp) (format "(hld-mul ~a ~a)" $1 $3)]
         [(nt-exp / nt-exp) (format "(hld-div ~a ~a)" $1 $3)]
         [(nt-exp < nt-exp) (format "(hld-lt ~a ~a)" $1 $3)]
         [(nt-exp > nt-exp) (format "(hld-gt ~a ~a)" $1 $3)]
         [(nt-exp % nt-exp) (format "(hld-mod ~a ~a)" $1 $3)]
         [(nt-exp GE nt-exp) (format "(hld-ge ~a ~a)" $1 $3)]
         [(nt-exp LE nt-exp) (format "(hld-le ~a ~a)" $1 $3)]
         [(! OP nt-exp CP) (format "(hld-not ~a)" $3)]
         [(OP nt-exp CP) $2]
         [(LII nt-exp) $2]))))

(define halide->rktlang-parser
  (parser

   (start start)
   (end newline EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))

   (precs (left OR AND)
          (right EQ NEQ)
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
         [(TVAR) $1]
         [(NTVAR) $1]
         [(TRUE) "true"]
         [(FALSE) "false"]
         [(UINT1) "true"]
         [(UINT0) "false"]
         [(exp EQ exp) (format "(eq? ~a ~a)" $1 $3)] ;; we don't use the halide-lang methods so we don't have to type args
         [(exp NEQ exp) (format "(not (eq? ~a ~a)" $1 $3)]
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
          (right EQ NEQ)
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
         [(TVAR) (hash-ref varmap (symbol->string $1))]
         [(NTVAR) (hash-ref varmap (symbol->string $1))]
         [(TRUE) "true"]
         [(FALSE) "false"]
         [(UINT1) "true"]
         [(UINT0) "false"]
         [(exp EQ exp) (format "(~a == ~a)" $1 $3)]
         [(exp NEQ exp) (format "(~a != ~a)" $1 $3)]
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
          (right EQ NEQ)
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
         [(TVAR) 0]
         [(NTVAR) 0]
         [(TRUE) 0]
         [(FALSE) 0]
         [(UINT1) 0]
         [(UINT0) 0]
         [(exp EQ exp) (+ 1 $1 $3)]
         [(exp NEQ exp) (+ 1 $1 $3)]
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

(define (halide-expr-in-solved-form? s)
  (with-handlers ([(λ (e) #t)
                   (λ (e) #f)])
    (evaluate-halide-parser halide->solvedform-parser s)))

(define (halide->termIR s)
  (evaluate-halide-parser halide->termIR-parser s))

#;(with-input-from-file "expr.txt"
  (λ ()
    (with-output-to-file "file.txt"
      (λ ()
        (for ([line (in-lines)])
         ; (println (halide->rktlang line))
          (display (format "(cons \"~a\" ~a)\n" line (halide->rktlang line)))
          )))))