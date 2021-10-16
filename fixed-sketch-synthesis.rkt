#lang rosette

(require "halide-lang.rkt")
(require "halide-parser.rkt")
(require "halide-sketch.rkt")
(require "varsolverTRS.rkt")
(require "traat/termIR.rkt")
(require "traat/matching.rkt")

(provide synthesize-from-fixed-metasketches)

(struct fixed-metasketch (sigma-term func op-count arg-count))
(struct fixed-sketch (metasketch op-list op-indexes nvar-indexes tvar-pos) #:transparent)

(define-symbolic* sym-tvar integer?)
(define-symbolic* sym-nvar1 integer?)
(define-symbolic* sym-nvar2 integer?)
(define-symbolic* sym-nvar3 integer?)
(define-symbolic* op-index0 integer?)
(define-symbolic* op-index1 integer?)
(define-symbolic* op-index2 integer?)
(define-symbolic* nvar-index0 integer?)
(define-symbolic* nvar-index1 integer?)
(define-symbolic* nvar-index2 integer?)

;;;; 1 op pattern
(define (1op-patt op arg1 arg2)
  (sigma-term op (list arg1 arg2)))
(define (1op-patt-func op arg1 arg2)
  (op arg1 arg2))

(define 1op-patt-metasketch (fixed-metasketch 1op-patt 1op-patt-func 1 2))

;;;; 2 op patterns
(define (2op-patt1 op1 op2 arg1 arg2 arg3)
  (sigma-term op1 (list arg1 (sigma-term op2 (list arg2 arg3)))))
(define (2op-patt1-func op1 op2 arg1 arg2 arg3)
  (op1 arg1 (op2 arg2 arg3)))

(define 2op-patt1-metasketch (fixed-metasketch 2op-patt1 2op-patt1-func 2 3))

(define (2op-patt2 op1 op2 arg1 arg2 arg3)
  (sigma-term op1 (list (sigma-term op2 (list arg1 arg2)) arg3)))
(define (2op-patt2-func op1 op2 arg1 arg2 arg3)
  (op1 (op2 arg1 arg2) arg3))

(define 2op-patt2-metasketch (fixed-metasketch 2op-patt2 2op-patt2-func 2 3))

;;;; 3 op patterns
(define (3op-patt1 op1 op2 op3 arg1 arg2 arg3 arg4)
  (sigma-term op1 (list (sigma-term op2 (list arg1 arg2)) (sigma-term op3 (list arg3 arg4)))))
(define (3op-patt1-func op1 op2 op3 arg1 arg2 arg3 arg4)
  (op1 (op2 arg1 arg2) (op3 arg3 arg4)))
(define 3op-patt1-metasketch (fixed-metasketch 3op-patt1 3op-patt1-func 3 4))

(define (3op-patt2 op1 op2 op3 arg1 arg2 arg3 arg4)
  (sigma-term op1 (list arg1 (sigma-term op2 (list arg2 (sigma-term op3 (list arg3 arg4)))))))
(define (3op-patt2-func op1 op2 op3 arg1 arg2 arg3 arg4)
  (op1 arg1 (op2 arg2 (op3 arg3 arg4))))
(define 3op-patt2-metasketch (fixed-metasketch 3op-patt2 3op-patt2-func 3 4))

(define (3op-patt3 op1 op2 op3 arg1 arg2 arg3 arg4)
  (sigma-term op1 (list arg1 (sigma-term op2 (list (sigma-term op3 (list arg2 arg3)) arg4)))))
(define (3op-patt3-func op1 op2 op3 arg1 arg2 arg3 arg4)
  (op1 arg1 (op2 (op3 arg2 arg3) arg4)))
(define 3op-patt3-metasketch (fixed-metasketch 3op-patt3 3op-patt3-func 3 4))

(define (3op-patt4 op1 op2 op3 arg1 arg2 arg3 arg4)
  (sigma-term op1 (list (sigma-term op2 (list arg1 (sigma-term op3 (list arg2 arg3)))) arg4)))
(define (3op-patt4-func op1 op2 op3 arg1 arg2 arg3 arg4)
  (op1 (op2 arg1 (op3 arg2 arg3)) arg4))
(define 3op-patt4-metasketch (fixed-metasketch 3op-patt4 3op-patt4-func 3 4))

(define (3op-patt5 op1 op2 op3 arg1 arg2 arg3 arg4)
  (sigma-term op1 (list (sigma-term op2 (list (sigma-term op3 (list arg1 arg2)) arg3)) arg4)))
(define (3op-patt5-func op1 op2 op3 arg1 arg2 arg3 arg4)
  (op1 (op2 (op3 arg1 arg2) arg3) arg4))
(define 3op-patt5-metasketch (fixed-metasketch 3op-patt5 3op-patt5-func 3 4))

(define all-fixed-metasketches
  (list 1op-patt-metasketch
        2op-patt1-metasketch
        2op-patt2-metasketch
        3op-patt1-metasketch
        3op-patt2-metasketch
        3op-patt3-metasketch
        3op-patt4-metasketch
        3op-patt5-metasketch))

(define (insert-target-var nv-list tarvar target-idx)
  (append (take nv-list target-idx) (list tarvar) (drop nv-list target-idx)))

(define (fixed-sketch-obeys-order? LHS fmetasketch tvar t-idx)
  (varsolver-reduction-order? (make-rule LHS
                                    (apply (apply curry (cons (fixed-metasketch-sigma-term fmetasketch) (map (λ (l) '+) (range (fixed-metasketch-op-count fmetasketch)))))
                                           (insert-target-var (map (λ (i) (format "n~a" i))
                                                                   (range (sub1 (fixed-metasketch-arg-count fmetasketch)))) tvar t-idx)))))

(define (eval-fixed-sketch fsketch tvar nvar-list)
  (let* ([operators (map (λ (idx) (get-operator-function-by-idx (fixed-sketch-op-list fsketch) idx)) (fixed-sketch-op-indexes fsketch))]
         [n-variables (map (λ (idx) (list-ref nvar-list idx)) (fixed-sketch-nvar-indexes fsketch))]
         [arguments (insert-target-var n-variables tvar (fixed-sketch-tvar-pos fsketch))])
    (apply (fixed-metasketch-func (fixed-sketch-metasketch fsketch)) (append operators arguments))))

(define (fixed-sketch->termIR fsketch tvar nvar-list)
    (let* ([operators (map (λ (idx) (get-operator-symbol-by-idx (fixed-sketch-op-list fsketch) idx)) (fixed-sketch-op-indexes fsketch))]
           [n-variables (map (λ (idx) (list-ref nvar-list idx)) (fixed-sketch-nvar-indexes fsketch))]
           [arguments (insert-target-var n-variables tvar (fixed-sketch-tvar-pos fsketch))])
      (apply (fixed-metasketch-sigma-term (fixed-sketch-metasketch fsketch)) (append operators arguments))))

(define (synthesize-from-fixed-metasketch LHS fmetasketch)
  (let* ([LHS-variables (termIR->variables LHS)]
         [target-variables (filter is-tvar-matching? LHS-variables)]
         [non-tvar-variables (filter (λ (v) (not (is-tvar-matching? v))) LHS-variables)]
         [valid-target-positions (filter (λ (i) (fixed-sketch-obeys-order? LHS fmetasketch (car target-variables) i))
                                         (range (fixed-metasketch-arg-count fmetasketch)))])
    (letrec ([f (λ (positions)
                  (begin
                    (clear-vc!)
                    (if (empty? positions)
                        (begin
                          (displayln "No rule found for fixed sketch")
                          'fail)
                        (let* ([sym-tvar (get-sym-int)]
                               [sym-nvars (map (λ (e) (get-sym-int)) non-tvar-variables)]
                               [fsketch (fixed-sketch fmetasketch operator-list (map (λ (e) (get-sym-int)) (range (fixed-metasketch-op-count fmetasketch)))
                                                      (map (λ (e) (get-sym-int))
                                                           (range (sub1 (fixed-metasketch-arg-count fmetasketch)))) (car positions))]
                               [evaled-LHS (apply (termIR->function LHS LHS-variables)
                                                  (insert-target-var sym-nvars sym-tvar (index-of LHS-variables (car target-variables))))]
                               [evaled-fixed-sketch  (eval-fixed-sketch fsketch sym-tvar sym-nvars)]
                               [model (time (with-handlers ([exn:fail:contract? (λ (e) (displayln (format "Function contract error ~a" (exn-message e))))]
                                                            [exn:fail? (λ (e) (displayln (format "Synthesis error ~a" (exn-message e))))])
                                              (synthesize #:forall (cons sym-tvar sym-nvars)
                                                          #:guarantee (assert (equal? evaled-fixed-sketch evaled-LHS)))))])
                          (if (or (void? model) (unsat? model) (unknown? model))
                              (begin
                                (displayln (format "Could not find solution for position ~a" (car positions)))
                                (f (cdr positions)))
                              (fixed-sketch->termIR (evaluate fsketch model) (car target-variables) non-tvar-variables))))))])
      (if (not (equal? (length target-variables) 1))
          (begin
            (displayln "Fixed sketches not implemented for patterns with >1 target variables")
            'fail)
          (f valid-target-positions)))))

(define (synthesize-from-fixed-metasketches LHS)
  (letrec ([f (λ (metasketches)
                (if (empty? metasketches)
                    (begin (displayln (format "No rule found for LHS pattern ~a" (termIR->halide LHS)))
                           'fail)
                    (let ([synthesis-output (synthesize-from-fixed-metasketch LHS (car metasketches))])
                      (if (equal? synthesis-output 'fail)
                          (f (cdr metasketches))
                          (make-rule LHS synthesis-output)))))])
    (f all-fixed-metasketches)))