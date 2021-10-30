#lang rosette

(require "typed-halide-lang.rkt")
(require "typed-halide-sketch.rkt")

(provide print-sketch print-topn-sketch topn-sketch->halide-expr sketch->halide-expr)

(define (print-sketch sk)
  (displayln (string-join (sketch->string sk) "\n")))

(define (sketch->string sk)
  (append (list (format "(define (sketch-function ~a)" (string-join (args->string-list sk) " ")))
          (inputs->string-list sk)
          (insns->string-list sk)
          (list (format "  R~a)" (sketch-retval-idx sk)))))

(define (args->string-list sk)
  (append (for/list ([i (range (sketch-input-count sk))]) (format "_~a" i))))

(define (inputs->string-list sk)
  (let ([args (args->string-list sk)])
    (for/list ([i (range (length args))])
      (format "  (define R~a ~a)" i (list-ref args i)))))

(define (insns->string-list sk)
  (let ([input-offset (sketch-input-count sk)])
    (for/list ([i (range (length (sketch-insn-list sk)))])
      (let ([current-insn (list-ref (sketch-insn-list sk) i)])
        (format "  (define R~a (~a ~a))" (+ input-offset i)
                (get-operator-name-by-idx (sketch-operator-list sk) (insn-op-idx current-insn)) (insn-args->string current-insn))))))

(define (insn-args->string i)
  (case (get-operator-arity-by-idx (insn-op-idx i))
    [(1) (format "R~a" (number->string (insn-arg1-idx i)))]
    [(2) (format "R~a R~a" (number->string (insn-arg1-idx i)) (number->string (insn-arg2-idx i)))]
    [(3) (format "R~a R~a R~a" (number->string (insn-arg1-idx i))
                 (number->string (insn-arg2-idx i)) (number->string (insn-arg3-idx i)))]
    ))

(define (print-topn-sketch sk op-idx)
  (displayln (string-join (topn-sketch->string sk op-idx) "\n")))

(define (topn-sketch->string sk op-idx)
  (let ([root-node-idx (+ (sketch-input-count sk) (length (sketch-insn-list sk)))])
    (append (list (format "(define (topn-sketch-function t0 ~a)" (string-join (args->string-list sk) " ")))
            (list "  (define RT t0)")
            (inputs->string-list sk)
            (insns->string-list sk)
            (list (format "  (define R~a (~a RT R~a))" root-node-idx (get-operator-name-by-idx (sketch-operator-list sk) op-idx) (sketch-retval-idx sk)))
            (list (format "  R~a)" root-node-idx)))))

(define (sketch->halide-expr sk variable-names)
  (letrec ([f (λ (i)
                (if (< i (sketch-input-count sk))
                    (list-ref variable-names i)
                    (let ([current-insn (list-ref (sketch-insn-list sk) (- i (sketch-input-count sk)))])
                      ((get-operator-string-function-by-idx (sketch-operator-list sk) (insn-op-idx current-insn))
                       (f (insn-arg1-idx current-insn))
                       (f (insn-arg2-idx current-insn))
                       (f (insn-arg3-idx current-insn))
                       ))))])
    (f (sketch-retval-idx sk))))

(define (topn-sketch->halide-expr sk root-op-idx target-var non-target-vars)
  ((get-operator-string-function-by-idx (sketch-operator-list sk) root-op-idx)
    target-var (sketch->halide-expr sk non-target-vars)))

#;(define (topn-sketch->halide-expr sk root-op-idx)
  (letrec ([f (λ (i)
              (if (< i (sketch-input-count sk))
                  (format "n~a" i)
                  (let ([current-insn (list-ref (sketch-insn-list sk) (- i (sketch-input-count sk)))])
                    ((get-operator-string-function-by-idx (sketch-operator-list sk) (insn-op-idx current-insn))
                     (f (insn-arg1-idx current-insn))
                     (f (insn-arg2-idx current-insn))
                     (f (insn-arg3-idx current-insn))))))])
    ((get-operator-string-function-by-idx (sketch-operator-list sk) root-op-idx) "t0" (f (sketch-retval-idx sk)))))