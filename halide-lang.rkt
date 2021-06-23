#lang rosette

(provide (all-defined-out))

(define (hld-true? x)
  (or (and (boolean? x) x)
      (and (integer? x) (not (equal? x 0)))))

(define (div-in-Z-val x y)
  (if (= (modulo x y) 0) 0 1))

(define (euclidean-div x y)
  (cond [(eq? y 0) 0]
        [(and (negative? x) (negative? y)) (+ (- (quotient (abs x) y)) (div-in-Z-val x y))]
        [(and (negative? x) (not (eq? y 0))) (- (- (quotient (abs x) y)) (div-in-Z-val x y))]
        [else (quotient x y)]))

(define (euclidean-mod x y)
  (cond [(eq? y 0) 0]
        [(and (negative? x) (negative? y)) (- (- (modulo (abs x) (abs y))) (* y (div-in-Z-val x y)))]
        [(and (not (eq? y 0)) (negative? x)) (+ (- (modulo (abs x) y)) (* y (div-in-Z-val x y)))]
        [(negative? y) (modulo x (abs y))]
        [else (modulo x y)]))
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

(define (hld-add i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (+ i1 i2)
      'error))

(define (hld-add->string i1 i2 [i3 ""])
  (format "(~a + ~a)" i1 i2))

(define (hld-sub i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (- i1 i2)
      'error))

(define (hld-sub->string i1 i2 [i3 ""])
  (format "(~a - ~a)" i1 i2))

(define (hld-mul i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (* i1 i2)
      'error))

(define (hld-mul->string i1 i2 [i3 ""])
  (format "(~a * ~a)" i1 i2))

(define (hld-min i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (min i1 i2)
      'error))

(define (hld-min->string i1 i2 [i3 ""])
  (format "min(~a, ~a)" i1 i2))

(define (hld-max i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (max i1 i2)
      'error))

(define (hld-max->string i1 i2 [i3 ""])
  (format "max(~a, ~a)" i1 i2))

(define (hld-div i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (euclidean-div i1 i2)
      'error))

(define (hld-div->string i1 i2 [i3 ""])
  (format "(~a / ~a)" i1 i2))

(define (hld-mod i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (euclidean-mod i1 i2)
      'error))

(define (hld-mod->string i1 i2 [i3 ""])
  (format "(~a % ~a)" i1 i2))

(define (hld-lt i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (< i1 i2)
      'error))

(define (hld-lt->string i1 i2 [i3 ""])
  (format "(~a < ~a)" i1 i2))

(define (hld-le i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (<= i1 i2)
      'error))

(define (hld-le->string i1 i2 [i3 ""])
  (format "(~a <= ~a)" i1 i2))

(define (hld-gt i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (> i1 i2)
      'error))

(define (hld-gt->string i1 i2 [i3 ""])
  (format "(~a > ~a)" i1 i2))

(define (hld-ge i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (>= i1 i2)
      'error))

(define (hld-ge->string i1 i2 [i3 0])
  (format "(~a >= ~a)" i1 i2))

(define (hld-eqi i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (eq? i1 i2)
      'error))

(define (hld-eq->string i1 i2 [i3 ""])
  (format "(~a == ~a)" i1 i2))

(define (hld-eqb i1 i2 [i3 #f])
  (if (and (boolean? i1) (boolean? i2))
      (eq? i1 i2)
      'error))

(define (hld-eqb->string i1 i2 [i3 ""])
  (format "(~a == ~a)" i1 i2))

(define (hld-neqi i1 i2 [i3 0])
  (if (and (integer? i1) (integer? i2))
      (not (eq? i1 i2))
      'error))

(define (hld-neq->string i1 i2 [i3 ""])
  (format "(~a != ~a)" i1 i2))

(define (hld-neqb i1 i2 [i3 #f])
  (if (and (boolean? i1) (boolean? i2))
      (not (eq? i1 i2))
      'error))

(define (hld-seli i1 i2 i3)
  (if (and (boolean? i1) (and (integer? i1) (integer? i2)))
      (if i1 i2 i3)
      'error))

(define (hld-sel->string i1 i2 i3)
  (format "select(~a, ~a, ~a)" i1 i2 i3))

(define (hld-selb i1 i2 i3)
  (if (and (boolean? i1) (and (boolean? i1) (boolean? i2)))
      (if i1 i2 i3)
      'error))

#;(define (hld-and i1 i2 [i3 #f])
  (if (and (boolean? i1) (boolean? i2))
      (and i1 i2)
      'error))

(define (hld-and i1 i2 [i3 #f])
  (and (hld-true? i1) (hld-true? i2)))

(define (hld-and->string i1 i2 [i3 ""])
  (format "(~a && ~a)" i1 i2))

#;(define (hld-or i1 i2 [i3 #f])
  (if (and (boolean? i1) (boolean? i2))
      (or i1 i2)
      'error))
(define (hld-or i1 i2 [i3 #f])
  (or (hld-true? i1) (hld-true? i2)))

(define (hld-or->string i1 i2 [i3 #f])
  (format "(~a || ~a)" i1 i2))

#;(define (hld-not i1 [i2 #f] [i3 #f])
  (if (boolean? i1)
      (not i1)
      'error))

(define (hld-not i1 [i2 #f] [i3 #f])
  (not (hld-true? i1)))

(define (hld-not->string i1 [i2 ""] [i3 ""])
  (format "!(~a)" i1))

(struct operator (function arity name string-function) #:transparent)

(define add-operator (operator hld-add 2 "hld-add" hld-add->string))
(define sub-operator (operator hld-sub 2 "hld-sub" hld-sub->string))
(define mod-operator (operator hld-mod 2 "hld-mod" hld-mod->string))
(define mul-operator (operator hld-mul 2 "hld-mul" hld-mul->string))
(define div-operator (operator hld-div 2 "hld-div" hld-div->string))
(define min-operator (operator hld-min 2 "hld-min" hld-min->string))
(define max-operator (operator hld-max 2 "hld-max" hld-max->string))
(define eqi-operator (operator hld-eqi 2 "hld-eqi" hld-eq->string))
(define neqi-operator (operator hld-neqi 2 "hld-neqi" hld-neq->string))
(define eqb-operator (operator hld-eqb 2 "hld-eqb" hld-eq->string))
(define neqb-operator (operator hld-neqb 2 "hld-neqb" hld-neq->string))
(define lt-operator (operator hld-lt 2 "hld-lt" hld-lt->string))
(define le-operator (operator hld-le 2 "hld-le" hld-le->string))
(define gt-operator (operator hld-gt 2 "hld-gt" hld-gt->string))
(define ge-operator (operator hld-ge 2 "hld-ge" hld-ge->string))
(define and-operator (operator hld-and 2 "hld-and" hld-and->string))
(define or-operator (operator hld-or 2 "hld-or" hld-or->string))
(define not-operator (operator hld-not 1 "hld-not" hld-not->string))
(define seli-operator (operator hld-seli 3 "hld-seli" hld-sel->string))
(define selb-operator (operator hld-selb 3 "hld-selb" hld-sel->string))

(define operator-list
  (list add-operator ;; 0
        sub-operator ;; 1
        mul-operator ;; 2
        div-operator ;; 3
        mod-operator ;; 4
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
      ;  seli-operator ;; 18
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
