#lang rosette

(require "typed-halide-lang.rkt")
(require "halide-parser.rkt")
(require "typed-halide-sketch.rkt")
(require "rule-orders.rkt")
(require "traat/termIR.rkt")
(require "traat/matching.rkt")

(provide (all-defined-out))

(struct fixed-metasketch (sigma-term func op-count arg-count max-depth) #:transparent)
;; op-indexes: for each operator slot, index into the list of operators that will fill it
;; tvar and nvar indexes: lists of indexes into the argument list that are target or non-target
(struct fixed-sketch (metasketch op-list op-indexes tvar-idxes) #:transparent)

;;;; 1 op pattern
(define (1op-patt op arg1 arg2)
  (sigma-term op (list arg1 arg2)))
(define (1op-patt-func op arg1 arg2)
  (op arg1 arg2))

(define 1op-patt-metasketch (fixed-metasketch 1op-patt 1op-patt-func 1 2 1))

;;;; 2 op patterns
(define (2op-patt1 op1 op2 arg1 arg2 arg3)
  (sigma-term op1 (list arg1 (sigma-term op2 (list arg2 arg3)))))
(define (2op-patt1-func op1 op2 arg1 arg2 arg3)
  (op1 arg1 (op2 arg2 arg3)))

(define 2op-patt1-metasketch (fixed-metasketch 2op-patt1 2op-patt1-func 2 3 2))

(define (2op-patt2 op1 op2 arg1 arg2 arg3)
  (sigma-term op1 (list (sigma-term op2 (list arg1 arg2)) arg3)))
(define (2op-patt2-func op1 op2 arg1 arg2 arg3)
  (op1 (op2 arg1 arg2) arg3))

(define 2op-patt2-metasketch (fixed-metasketch 2op-patt2 2op-patt2-func 2 3 2))

;;;; 3 op patterns
(define (3op-patt1 op1 op2 op3 arg1 arg2 arg3 arg4)
  (sigma-term op1 (list (sigma-term op2 (list arg1 arg2)) (sigma-term op3 (list arg3 arg4)))))
(define (3op-patt1-func op1 op2 op3 arg1 arg2 arg3 arg4)
  (op1 (op2 arg1 arg2) (op3 arg3 arg4)))
(define 3op-patt1-metasketch (fixed-metasketch 3op-patt1 3op-patt1-func 3 4 2))

(define (3op-patt2 op1 op2 op3 arg1 arg2 arg3 arg4)
  (sigma-term op1 (list arg1 (sigma-term op2 (list arg2 (sigma-term op3 (list arg3 arg4)))))))
(define (3op-patt2-func op1 op2 op3 arg1 arg2 arg3 arg4)
  (op1 arg1 (op2 arg2 (op3 arg3 arg4))))
(define 3op-patt2-metasketch (fixed-metasketch 3op-patt2 3op-patt2-func 3 4 3))

(define (3op-patt3 op1 op2 op3 arg1 arg2 arg3 arg4)
  (sigma-term op1 (list arg1 (sigma-term op2 (list (sigma-term op3 (list arg2 arg3)) arg4)))))
(define (3op-patt3-func op1 op2 op3 arg1 arg2 arg3 arg4)
  (op1 arg1 (op2 (op3 arg2 arg3) arg4)))
(define 3op-patt3-metasketch (fixed-metasketch 3op-patt3 3op-patt3-func 3 4 3))

(define (3op-patt4 op1 op2 op3 arg1 arg2 arg3 arg4)
  (sigma-term op1 (list (sigma-term op2 (list arg1 (sigma-term op3 (list arg2 arg3)))) arg4)))
(define (3op-patt4-func op1 op2 op3 arg1 arg2 arg3 arg4)
  (op1 (op2 arg1 (op3 arg2 arg3)) arg4))
(define 3op-patt4-metasketch (fixed-metasketch 3op-patt4 3op-patt4-func 3 4 3))

(define (3op-patt5 op1 op2 op3 arg1 arg2 arg3 arg4)
  (sigma-term op1 (list (sigma-term op2 (list (sigma-term op3 (list arg1 arg2)) arg3)) arg4)))
(define (3op-patt5-func op1 op2 op3 arg1 arg2 arg3 arg4)
  (op1 (op2 (op3 arg1 arg2) arg3) arg4))
(define 3op-patt5-metasketch (fixed-metasketch 3op-patt5 3op-patt5-func 3 4 3))

(define all-fixed-metasketches
  (list 1op-patt-metasketch
        2op-patt1-metasketch
        2op-patt2-metasketch
        3op-patt1-metasketch
        3op-patt2-metasketch
        3op-patt3-metasketch
        3op-patt4-metasketch
        3op-patt5-metasketch))

(define 1op-metasketches (list 1op-patt-metasketch))
(define 2op-metasketches (list 2op-patt1-metasketch 2op-patt2-metasketch))
(define 3op-metasketches (list 3op-patt1-metasketch
                               3op-patt2-metasketch
                               3op-patt3-metasketch
                               3op-patt4-metasketch
                               3op-patt5-metasketch))

#;(define (insert-target-var nv-list tarvar target-idx)
  (append (take nv-list target-idx) (list tarvar) (drop nv-list target-idx)))

(define (interleave-arguments tvars nvars tvar-idxes)
  (let ([nvar-idxes (filter (λ (i) (not (member i tvar-idxes))) (range (+ (length tvars) (length nvars))))])
    (map (λ (i) (if (member i tvar-idxes) (list-ref tvars (index-of tvar-idxes i))
                    (list-ref nvars (index-of nvar-idxes i)))) (range (+ (length tvars) (length nvars))))))

(define (fixed-sketch-obeys-order? LHS fmetasketch tvar-idxes)
  (let* ([LHS-terminal-instances (termIR->terminal-instances LHS)]
         [tvars (filter (λ (t) (and (term-variable? t) (is-tvar-matching? t))) LHS-terminal-instances)]
         [nvars (filter (λ (t) (not (and (term-variable? t) (is-tvar-matching? t)))) LHS-terminal-instances)])
  (varsolver-reduction-order? (make-rule LHS
                                         (apply (fixed-metasketch-sigma-term fmetasketch)
                                                (append (map (λ (l) '+) (range (fixed-metasketch-op-count fmetasketch)))
                                                        (interleave-arguments tvars nvars tvar-idxes)))))))

(define (sort-target-positions fmetasketch tvars nvars positions)
  (let* ([get-dummy-term (λ (tvar-idxes)
                          (apply (fixed-metasketch-sigma-term fmetasketch)
                                 (append (map (λ (l) '+) (range (fixed-metasketch-op-count fmetasketch)))
                                         (interleave-arguments tvars nvars tvar-idxes))))])
    (sort positions (λ (p1 p2) (varsolver-reduction-order? (make-rule (get-dummy-term p2) (get-dummy-term p1)))))))

(define (eval-fixed-sketch fsketch tvars nvars)
  (let* ([fmetasketch (fixed-sketch-metasketch fsketch)]
         [operators (map (λ (idx) (get-operator-function-by-idx (fixed-sketch-op-list fsketch) idx)) (fixed-sketch-op-indexes fsketch))]
         [arguments (interleave-arguments tvars nvars (fixed-sketch-tvar-idxes fsketch))])
    (register-value (apply (fixed-metasketch-func (fixed-sketch-metasketch fsketch)) (append operators (map get-register arguments))))))

(define (fixed-sketch->termIR fsketch tvars nvars)
  (let* ([fmetasketch (fixed-sketch-metasketch fsketch)]
         [operators (map (λ (idx) (get-operator-symbol-by-idx (fixed-sketch-op-list fsketch) idx)) (fixed-sketch-op-indexes fsketch))]
         [arguments (interleave-arguments tvars nvars (fixed-sketch-tvar-idxes fsketch))])
    (apply (fixed-metasketch-sigma-term (fixed-sketch-metasketch fsketch)) (append operators arguments))))

(define 2tvar-target-positions
  (make-hash (list (cons 3 (list '(0 1) '(0 2) '(1 2)))
                   (cons 4 (list '(0 1) '(0 2) '(0 3) '(1 2) '(1 3) '(2 3))))))

(define 3tvar-target-positions
  (list '(0 1 2) '(0 1 3) '(1 2 3)))

(define (find-valid-tvar-positions LHS tvar-count fmetasketch)
  (let* ([arg-count (fixed-metasketch-arg-count fmetasketch)]
         [tvar-lists (if (equal? tvar-count 1)
                         (map (λ (x) (list x)) (range (fixed-metasketch-arg-count fmetasketch)))
                         (if (equal? tvar-count 2)
                             (hash-ref 2tvar-target-positions arg-count)
                             3tvar-target-positions))])
    (filter (λ (tvar-idxes) (fixed-sketch-obeys-order? LHS fmetasketch tvar-idxes)) tvar-lists)))

(define (synthesize-from-fixed-metasketch LHS fmetasketch)
  (let* ([LHS-terminal-instances (termIR->terminal-instances LHS)]
         [LHS-tvar-positions (filter (λ (i) (let ([t (list-ref LHS-terminal-instances i)])
                                                   (and (term-variable? t) (is-tvar-matching? t)))) (range (length LHS-terminal-instances)))]
         [target-variables (filter (λ (t) (and (term-variable? t) (is-tvar-matching? t))) LHS-terminal-instances)]
         [non-tvar-terminals (filter (λ (t) (not (and (term-variable? t) (is-tvar-matching? t)))) LHS-terminal-instances)]
         [constants (filter term-constant? non-tvar-terminals)]
         [valid-target-positions (sort-target-positions fmetasketch target-variables non-tvar-terminals (find-valid-tvar-positions LHS (length target-variables) fmetasketch))]
         [bound (if (current-bitwidth) (overflow-bounds (current-bitwidth) (fixed-metasketch-max-depth fmetasketch)) #f)])
    (if (and (current-bitwidth)
             (not (empty? constants))
             (or (> (apply max constants) bound)
                 (< (apply min constants) (- bound))))
        (begin
          (displayln (format "Constants in LHS ~a lie outside bounds (~a, ~a) for this metasketch" (termIR->halide LHS) bound (- bound)))
          'fail)
        (letrec ([f (λ (positions)
                      (begin
                        (clear-vc!)
                        (if (empty? positions)
                            (begin (displayln "No rule found for fixed sketch") 'fail)
                            (let* ([sym-tvars (map (λ (e) (get-sym-input-int)) target-variables)]
                                   [sym-nvars (map (λ (n) (if (term-constant? n) n (get-sym-input-int))) non-tvar-terminals)]
                                   [fsketch (fixed-sketch fmetasketch operator-list (map (λ (e) (get-sym-int)) (range (fixed-metasketch-op-count fmetasketch))) (car positions))]
                                   [evaled-LHS (apply (termIR->function LHS LHS-terminal-instances) (interleave-arguments sym-tvars sym-nvars LHS-tvar-positions))]
                                   [evaled-fixed-sketch (eval-fixed-sketch fsketch sym-tvars sym-nvars)]
                                   [model (begin
                                            (unless (not (current-bitwidth))
                                              (for ([v (append sym-tvars (filter symbolic? sym-nvars))])
                                                             (assume (bvsle v (bv bound (current-bitwidth))))
                                                             (assume (bvsge v (bv (- bound) (current-bitwidth))))))
                                            (time (with-handlers ([exn:fail:contract? (λ (e) (displayln (format "Function contract error ~a" (exn-message e))))]
                                                                  [exn:fail? (λ (e) (displayln (format "Synthesis error ~a" (exn-message e))))])
                                                    (synthesize #:forall (append sym-tvars (filter symbolic? sym-nvars))
                                                                #:guarantee (assert (equal? evaled-fixed-sketch evaled-LHS))))))]) 
                              (if (or (void? model) (unsat? model) (unknown? model))
                                  (begin
                                (displayln (format "Could not find solution for position ~a" (car positions)))
                                (f (cdr positions)))
                                  (fixed-sketch->termIR (evaluate fsketch model) target-variables non-tvar-terminals))))))])
          (if (> (length target-variables) 3)
              (begin (displayln "Fixed sketches not implemented for patterns with >1 target variables") 'fail)
              (f valid-target-positions))))))

(define (synthesize-from-fixed-metasketches LHS)
  (letrec ([f (λ (metasketches)
                (if (empty? metasketches)
                    (begin (displayln (format "No rule found for LHS pattern ~a" (termIR->halide LHS)))
                           'fail)
                    (let ([synthesis-output (synthesize-from-fixed-metasketch LHS (car metasketches))])
                      (if (equal? synthesis-output 'fail)
                          (f (cdr metasketches))
                          (make-rule LHS synthesis-output)))))])
    (let ([tvar-instance-count (length (filter (λ (t) (and (term-variable? t) (is-tvar-matching? t))) (termIR->terminal-instances LHS)))])
    (if (> tvar-instance-count 3)
        (displayln "Fixed sketches not implemented for patterns with >3 target variables")
        (f (filter (λ (m) (and (equal? (length (termIR->terminal-instances LHS)) (fixed-metasketch-arg-count m))
                               (< (length (filter (λ (t) (and (term-variable? t) (is-tvar-matching? t))) (termIR->terminal-instances LHS)))
                                  (fixed-metasketch-arg-count m)))) all-fixed-metasketches))))))


;;;; broken rules synthesized
;;  "(n0 - t1) -> (t1 - n0) [unknownorder]"
;;  "((t0 - n1) + t2) -> (t0 - (t2 - n1)) [unknownorder]"
;;  "((t0 * n1) - t2) -> ((t0 * t2) - n1) [unknownorder]"
;;  "((t0 - n1) * t2) -> ((t0 - t2) * n1) [unknownorder]")