#lang rosette

(provide (all-defined-out))

;; all operators take registers as inputs
;; type can be 'int 'bool or 'error
;; value can be a variable, a constant, or another register expression
(struct register (type value) #:transparent)

(define (int-register? reg)
  (eq? (register-type reg) 'int))
(define (bool-register? reg)
  (eq? (register-type reg) 'bool))
(define (error-register? reg)
  (eq? (register-type reg) 'error))

(define error-register (register 'error 'error))

(define (bvabs x)
  (if (bvslt x (bvxor x x))
      (bvneg x)
      x))

(define (bvpositive? x)
  (bvslt (bvsub x x) x))
(define (bvnegative? x)
  (bvslt x (bvsub x x)))

(define (div-in-Z-val x y)
  (if (= (modulo x y) 0) 0 1))

(define (euclidean-div x y)
  (cond [(eq? y 0) 0]
        [(and (negative? x) (negative? y)) (+ (- (quotient (abs x) y)) (div-in-Z-val x y))]
        [(and (negative? x) (not (eq? y 0))) (- (- (quotient (abs x) y)) (div-in-Z-val x y))]
        [else (quotient x y)]))

(define (bv-euclidean-div x y)
  (cond [(bvzero? x) x]
        [(bvzero? y) y]
        [(and (bvpositive? x) (bvpositive? y)) (bvsdiv x y)]
        [(and (bvpositive? x) (bvnegative? y)) (bvneg (bvsdiv x (bvneg y)))]
        [(and (bvnegative? x) (bvpositive? y) (bvzero? (bvsmod x y))) (bvneg (bvsdiv (bvneg x) y))]
        [(and (bvnegative? x) (bvpositive? y) (not (bvzero? (bvsmod x y)))) (bvsub1 (bvneg (bvsdiv (bvneg x) y)))]
        [(and (bvnegative? x) (bvnegative? y) (bvzero? (bvsmod x y))) (bvsdiv (bvneg x) (bvneg y))]
        [(and (bvnegative? x) (bvnegative? y) (not (bvzero? (bvsmod x y)))) (bvadd1 (bvsdiv (bvneg x) (bvneg y)))]))

(define (euclidean-mod x y)
  (cond [(eq? y 0) 0]
        [(and (negative? x) (negative? y)) (- (- (modulo (abs x) (abs y))) (* y (div-in-Z-val x y)))]
        [(and (not (eq? y 0)) (negative? x)) (+ (- (modulo (abs x) y)) (* y (div-in-Z-val x y)))]
        [(negative? y) (modulo x (abs y))]
        [else (modulo x y)]))

(define (bv-euclidean-mod x y)
  (cond [(bvzero? x) x]
        [(bvzero? y) y]
        [(bvzero? (bvsmod x y)) (bvsmod x y)]
        [(and (bvpositive? x) (bvpositive? y)) (bvsmod x y)]
        [(and (bvpositive? x) (bvnegative? y)) (bvsmod x (bvneg y))]
        [(and (bvnegative? x) (bvpositive? y) (not (bvzero? (bvsmod x y)))) (bvsub y (bvsmod (bvneg x) y))]
        [(and (bvnegative? x) (bvnegative? y) (not (bvzero? (bvsmod x y)))) (bvsub (bvneg y) (bvsmod (bvneg x) (bvneg y)))]))

;; int -> int -> int
(define (int-int-int-func f r1 r2)
  (if (and (int-register? r1) (int-register? r2))
      (register 'int (f (register-value r1) (register-value r2)))
      error-register))

(define (hld-bv-add i1 i2 [i3 0])
  (int-int-int-func bvadd i1 i2))

(define (hld-add->string i1 i2 [i3 ""])
  (format "(~a + ~a)" i1 i2))

(define (hld-bv-sub i1 i2 [i3 0])
  (int-int-int-func bvsub i1 i2))

(define (hld-sub->string i1 i2 [i3 ""])
  (format "(~a - ~a)" i1 i2))

(define (hld-bv-mul i1 i2 [i3 0])
  (int-int-int-func bvmul i1 i2))

(define (hld-mul->string i1 i2 [i3 ""])
  (format "(~a * ~a)" i1 i2))

(define (hld-bv-min i1 i2 [i3 0])
  (int-int-int-func bvsmin i1 i2))

(define (hld-min->string i1 i2 [i3 ""])
  (format "min(~a, ~a)" i1 i2))

(define (hld-bv-max i1 i2 [i3 0])
  (int-int-int-func bvsmax i1 i2))

(define (hld-max->string i1 i2 [i3 ""])
  (format "max(~a, ~a)" i1 i2))

(define (hld-bv-div i1 i2 [i3 0])
  (int-int-int-func bv-euclidean-div i1 i2))

(define (hld-div->string i1 i2 [i3 ""])
  (format "(~a / ~a)" i1 i2))

(define (hld-bv-mod i1 i2 [i3 0])
  (int-int-int-func bv-euclidean-mod i1 i2))

(define (hld-mod->string i1 i2 [i3 ""])
  (format "(~a % ~a)" i1 i2))

;; int -> int -> bool
(define (int-int-bool-func f r1 r2)
  (if (and (int-register? r1) (int-register? r2))
      (register 'bool (f (register-value r1) (register-value r2)))
      error-register))

(define (hld-bv-lt i1 i2 [i3 0])
  (int-int-bool-func bvslt i1 i2))

(define (hld-lt->string i1 i2 [i3 ""])
  (format "(~a < ~a)" i1 i2))

(define (hld-bv-le i1 i2 [i3 0])
  (int-int-bool-func bvsle i1 i2))

(define (hld-le->string i1 i2 [i3 ""])
  (format "(~a <= ~a)" i1 i2))

(define (hld-bv-gt i1 i2 [i3 0])
  (int-int-bool-func bvsgt i1 i2))

(define (hld-gt->string i1 i2 [i3 ""])
  (format "(~a > ~a)" i1 i2))

(define (hld-bv-ge i1 i2 [i3 0])
  (int-int-bool-func bvsge i1 i2))

(define (hld-ge->string i1 i2 [i3 0])
  (format "(~a >= ~a)" i1 i2))

(define (hld-bv-eqi i1 i2 [i3 0])
  (int-int-bool-func bveq i1 i2))

(define (hld-eq->string i1 i2 [i3 ""])
  (format "(~a == ~a)" i1 i2))

(define (hld-bv-neqi i1 i2 [i3 0])
  (int-int-bool-func (λ (x y) (bvnot (bveq x y))) i1 i2))

(define (hld-neq->string i1 i2 [i3 ""])
  (format "(~a != ~a)" i1 i2))

;; bool -> int -> int -> int
(define (hld-bv-seli i1 i2 i3)
  (if (and (bool-register? i1) (and (int-register? i2) (int-register? i3)))
      (register 'int (if (register-value i1) (register-value i2) (register-value i3)))
      error-register))

(define (hld-sel->string i1 i2 i3)
  (format "select(~a, ~a, ~a)" i1 i2 i3))

;; bool -> bool -> bool -> bool
(define (hld-bv-selb i1 i2 i3)
  (if (and (bool-register? i1) (and (bool-register? i2) (bool-register? i3)))
      (register 'bool (register-value i1) (register-value i2) (register-value i3))
      error-register))

;; bool -> bool -> bool
(define (bool-bool-bool-func f r1 r2)
  (if (and (bool-register? r1) (bool-register? r2))
      (register 'bool (f (register-value r1) (register-value r2)))
      error-register))

(define (hld-bv-and i1 i2 [i3 #f])
  (bool-bool-bool-func bvand i1 i2))

(define (hld-and->string i1 i2 [i3 ""])
  (format "(~a && ~a)" i1 i2))

(define (hld-bv-or i1 i2 [i3 #f])
  (bool-bool-bool-func bvor i1 i2))

(define (hld-or->string i1 i2 [i3 #f])
  (format "(~a || ~a)" i1 i2))

(define (hld-bv-eqb i1 i2 [i3 #f])
  (bool-bool-bool-func bveq i1 i2))

(define (hld-bv-neqb i1 i2 [i3 #f])
   (bool-bool-bool-func (λ (x y) (not (eq? x y))) i1 i2))

;; bool -> bool
;; only one of these
(define (hld-bv-not i1 [i2 #f] [i3 #f])
  (if (bool-register? i1)
      (register 'bool (bvnot (register-value i1)))
      error-register))

(define (hld-not->string i1 [i2 ""] [i3 ""])
  (format "!(~a)" i1))

(struct operator (function arity name string-function op-symbol) #:transparent)

(define add-operator (operator hld-bv-add 2 "hld-add" hld-add->string '+))
(define sub-operator (operator hld-bv-sub 2 "hld-sub" hld-sub->string '-))
(define mod-operator (operator hld-bv-mod 2 "hld-mod" hld-mod->string '%))
(define mul-operator (operator hld-bv-mul 2 "hld-mul" hld-mul->string '*))
(define div-operator (operator hld-bv-div 2 "hld-div" hld-div->string '/))
(define min-operator (operator hld-bv-min 2 "hld-min" hld-min->string 'min))
(define max-operator (operator hld-bv-max 2 "hld-max" hld-max->string 'max))
(define eqi-operator (operator hld-bv-eqi 2 "hld-eqi" hld-eq->string '==))
(define neqi-operator (operator hld-bv-neqi 2 "hld-neqi" hld-neq->string '!=))
(define eqb-operator (operator hld-bv-eqb 2 "hld-eqb" hld-eq->string '==))
(define neqb-operator (operator hld-bv-neqb 2 "hld-neqb" hld-neq->string '!=))
(define lt-operator (operator hld-bv-lt 2 "hld-lt" hld-lt->string '<))
(define le-operator (operator hld-bv-le 2 "hld-le" hld-le->string '<=))
(define gt-operator (operator hld-bv-gt 2 "hld-gt" hld-gt->string '>))
(define ge-operator (operator hld-bv-ge 2 "hld-ge" hld-ge->string '>=))
(define and-operator (operator hld-bv-and 2 "hld-and" hld-and->string '&&))
(define or-operator (operator hld-bv-or 2 "hld-or" hld-or->string 'or))
(define not-operator (operator hld-bv-not 1 "hld-not" hld-not->string '!))
(define seli-operator (operator hld-bv-seli 3 "hld-seli" hld-sel->string 'select))
(define selb-operator (operator hld-bv-selb 3 "hld-selb" hld-sel->string 'select))

(define operator-list
  (list add-operator ;; 0
        sub-operator ;; 1
        mul-operator ;; 2
        div-operator ;; 3
        mod-operator ;; 4
        min-operator ;; 5 / 3
        max-operator ;; 6 / 4
        eqi-operator ;; 7 / 5
     ;   eqb-operator ;; 8
        neqi-operator ;; 9 / 6
     ;   neqb-operator ;; 10
        lt-operator ;; 11 / 7
        le-operator ;; 12 / 8
        gt-operator ;; 13 / 9
        ge-operator ;; 14 / 10
        and-operator ;; 15 / 11
        or-operator ;; 16 / 12
        not-operator ;; 17 / 13
        seli-operator ;; 18
      ;  selb-operator ;; 19
        ))

(define cheap-operator-list
  (list add-operator ;; 0
        sub-operator ;; 1
        mul-operator ;; 2
      ;  div-operator ;; 3
      ;  mod-operator ;; 4
        min-operator ;; 5
        max-operator ;; 6
        eqi-operator ;; 7
        eqb-operator ;; 8
        neqi-operator ;; 9
        neqb-operator ;; 10
        lt-operator ;; 11
        le-operator ;; 12
        gt-operator ;; 13
        ge-operator ;; 14
        and-operator ;; 15
        or-operator ;; 16
        not-operator ;; 17
     ;   seli-operator ;; 18
     ;   selb-operator ;; 19
        ))

(define int-operator-list
  (list add-operator ;; 0
        sub-operator ;; 1
        mul-operator ;; 2
        min-operator ;; 3
        max-operator ;; 4
        eqi-operator ;; 5
        neqi-operator ;; 6
        lt-operator ;; 7
        le-operator ;; 8
        gt-operator ;; 9
        ge-operator ;; 10
))

(define (get-operator-by-idx operator-list idx)
  (list-ref operator-list idx))
(define (get-operator-arity-by-idx operator-list idx)
  (operator-arity (get-operator-by-idx operator-list idx)))
(define (get-operator-name-by-idx operator-list idx)
  (operator-name (get-operator-by-idx operator-list idx)))
(define (get-operator-function-by-idx operator-list idx)
  (operator-function (get-operator-by-idx operator-list idx)))
(define (get-operator-string-function-by-idx operator-list idx)
  (operator-string-function (get-operator-by-idx operator-list idx)))
(define (get-operator-symbol-by-idx operator-list idx)
  (operator-op-symbol (get-operator-by-idx operator-list idx)))

;
;        result_int = select(op == 0, arg1_int, result_int);
;        result_bool = select(op == 0, arg1_bool, result_bool);
;        result_int = select(op == 1, arg1_int + arg2_int, result_int);
;        result_int = select(op == 2, arg1_int - arg2_int, result_int);
;        result_int = select(op == 3, arg1_int * arg2_int, result_int);
;        result_int = select(op == 4, min(arg1_int, arg2_int), result_int);
;        result_int = select(op == 5, max(arg1_int, arg2_int), result_int);
;        result_bool = select(op == 6, arg1_int < arg2_int, result_bool);
;        result_bool = select(op == 7, arg1_int <= arg2_int, result_bool);
;        result_bool = select(op == 8, arg1_int == arg2_int, result_bool);
;        result_bool = select(op == 9, arg1_int != arg2_int, result_bool);
;
;        // TODO: switch 2 to any constant divisor already found in the input
;        result_int = select(op == 10, arg1_int / 2, result_int);
;        result_int = select(op == 11, arg1_int % 2, result_int);
;
;        // Meaningful if arg1 is a bool
;        result_int = select(op == 12, select(arg1_bool, arg2_int, arg3_int), result_int);
;        result_bool = select(op == 13, arg1_bool && arg2_bool, result_bool);
;        result_bool = select(op == 14, arg1_bool || arg2_bool, result_bool);
;        result_bool = select(op == 15, !arg1_bool, result_bool);
;        result_bool = select(op == 16, arg1_bool, result_bool);