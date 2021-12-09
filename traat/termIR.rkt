#lang racket

(provide (struct-out sigma-term))
(provide (struct-out rule))
(provide (struct-out eq-identity))
(provide make-rule make-identity term-constant? term-variable? termIR->halide
         termIR->variables termIR->variable-instances termIR->in-solved-form? termIR->renamevars
         rename-to-fresh-vars term-size term-op-count cap-and-sort-terms-by-size
         contains-target-variable? is-tvar-matching?
         is-non-tvar-matching? is-general-matching?
         rename-to-tarvar-aware-vars same-matching-type?
         rename-to-tarvar-aware-term-pairs
         can-match-tvar? can-match-non-tvar? can-match-var-to-term?
         termIR->rule-in-solved-form? termIR->replace-constant-variables
         equal-mod-alpha-renaming? member-mod-alpha-renaming? contains-operator?
         termIR->typechecks?)

;; terms are vname/variables, integers, or sigma-terms
;; let's make variables strings for now

;; subst is a hash-table mapping variables to terms
(struct subst (mapping) #:transparent)

;; a rule has a lefthand side, a righthand side and a name
(struct rule (lhs rhs name) #:transparent)

(define (make-rule lhs rhs)
  (rule lhs rhs ""))

(struct trs (ruleset rule->string order-hash))

(struct eq-identity (lhs rhs name) #:transparent)

(define (make-identity t1 t2)
  (eq-identity t1 t2 ""))

;; terms can be variables (string representation), integers, booleans (symbols 'true and 'false), or sigma-terms

;; symbol representing Halide function symbol, followed by list of term arguments
(struct sigma-term (symbol term-list) #:transparent)

(define (get-sigma-term-type sterm)
  (if (equal? 'select (sigma-term-symbol sterm))
      'unknown ;; we could recursively check the select args, but do the cheap thing for now
      (if (member (sigma-term-symbol sterm) '(max min + - * / %))
          'integer
          'boolean)))

(define (term-constant? t)
  (or (integer? t) (equal? t 'true) (equal? t 'false)))

(define (term-variable? t)
  (string? t))

(define (termIR->halide t)
  (letrec ([f (λ (tprime)
                (cond [(integer? tprime) (number->string tprime)]
                      [(symbol? tprime) (symbol->string tprime)]
                      [(string? tprime) tprime]
                      [(and (sigma-term? tprime)
                            (or (equal? (sigma-term-symbol tprime) 'max)
                                (equal? (sigma-term-symbol tprime) 'min))) (format "~a(~a, ~a)"
                                                                                   (sigma-term-symbol tprime)
                                                                                   (f (list-ref (sigma-term-term-list tprime) 0))
                                                                                   (f (list-ref (sigma-term-term-list tprime) 1)))]
                      [(and (sigma-term? tprime)
                            (equal? (sigma-term-symbol tprime) 'select)) (format "select(~a, ~a, ~a)"
                                                                                 (f (list-ref (sigma-term-term-list tprime) 0))
                                                                                 (f (list-ref (sigma-term-term-list tprime) 1))
                                                                                 (f (list-ref (sigma-term-term-list tprime) 2)))]
                      [(and (sigma-term? tprime)
                            (equal? (sigma-term-symbol tprime) '!)) (format "!(~a)" (f (list-ref (sigma-term-term-list tprime) 0)))]
                      [(and (sigma-term? tprime)
                            (equal? (sigma-term-symbol tprime) 'or)) (format "(~a || ~a)" (f (list-ref (sigma-term-term-list tprime) 0))
                                                                            (f (list-ref (sigma-term-term-list tprime) 1)))]
                      [else (format "(~a ~a ~a)"
                                   (f (list-ref (sigma-term-term-list tprime) 0))
                                   (sigma-term-symbol tprime)
                                   (f (list-ref (sigma-term-term-list tprime) 1)))]))])
    (f t)))

(define (uniq l)
  (letrec ([f (λ (uniq-lst lst)
                (cond [(empty? lst) uniq-lst]
                      [(member (car lst) uniq-lst) (f uniq-lst (cdr lst))]
                      [else (f (append uniq-lst (list (car lst))) (cdr lst))]))])
    (f '() l)))

(define (termIR->variables t)
  (remove-duplicates (termIR->variable-instances t)))

(define (termIR->variable-instances t)
  (letrec ([f (λ (tprime)
                (cond [(string? tprime) (list tprime)]
                      [(sigma-term? tprime) (map f (sigma-term-term-list tprime))]
                      [else '()]))])
    (flatten (f t))))

(define (is-target-variable? v)
  (string-prefix? v "t"))

(define (termIR->in-solved-form? term tvar)
  (or (and (term-variable? term) (equal? term tvar))
      (andmap (λ (v) (not (equal? tvar v))) (termIR->variables term))
      (and (sigma-term? term)
           (and (equal? (list-ref (sigma-term-term-list term) 0) tvar)
                (andmap (λ (v) (not (equal? tvar v))) (flatten (map termIR->variables (cdr (sigma-term-term-list term)))))))))

(define (termIR->rule-in-solved-form? term)
  (or (and (term-variable? term) (is-tvar-matching? term))
      (andmap is-non-tvar-matching? (termIR->variables term))
      (and (sigma-term? term)
           (and (term-variable? (list-ref (sigma-term-term-list term) 0))
                (is-tvar-matching? (list-ref (sigma-term-term-list term) 0))
                (andmap is-non-tvar-matching? (flatten (map termIR->variables (cdr (sigma-term-term-list term)))))))))

(define (termIR->renamevars term varmap)
  (letrec ([f (λ (tprime)
                (cond [(term-variable? tprime) (hash-ref varmap tprime)]
                      [(term-constant? tprime) tprime]
                      [else (sigma-term (sigma-term-symbol tprime) (map f (sigma-term-term-list tprime)))]))])
    (f term)))

(define (rename-to-fresh-vars term varmap)
  (letrec ([f (λ (tprime)
                (cond [(and (term-variable? tprime) (hash-has-key? varmap tprime)) (hash-ref varmap tprime)]
                      [(and (term-variable? tprime) (not (hash-has-key? varmap tprime))) (begin
                                                                                           (hash-set! varmap tprime (format "v~a" (hash-count varmap)))
                                                                                           (hash-ref varmap tprime))]
                      [(term-constant? tprime) tprime]
                      [else (sigma-term (sigma-term-symbol tprime) (map f (sigma-term-term-list tprime)))]))])
    (cons varmap (f term))))

(define (termIR->replace-constant-variables term)
  (let ([varmap (make-hash '())])
    (letrec ([f (λ (tprime)
                  (cond [(term-variable? tprime) tprime]
                        [(term-constant? tprime) (begin
                                                   (unless (hash-has-key? varmap tprime)
                                                     (hash-set! varmap tprime (format "c~a" (hash-count varmap))))
                                                   (hash-ref varmap tprime))]
                      [else (sigma-term (sigma-term-symbol tprime) (map f (sigma-term-term-list tprime)))]))])
      (f term))))

(define (alpha-rename-vars t)
  (cdr (rename-to-tarvar-aware-vars t (make-hash '()) (list "talpha" "nalpha" "valpha"))))

(define (equal-mod-alpha-renaming? t1 t2)
  (equal? (alpha-rename-vars t1) (alpha-rename-vars t2)))

(define (member-mod-alpha-renaming? t tlist)
  (let ([renamed-t (alpha-rename-vars t)])
    (findf (λ (tprime)
             (let ([renamed-t2 (alpha-rename-vars tprime)])
               (equal? renamed-t renamed-t2))) tlist)))

(define (is-tvar-matching? v)
  (string-prefix? v "t"))
(define (is-non-tvar-matching? v)
  (string-prefix? v "n"))
(define (is-general-matching? v)
  (and (not (is-tvar-matching? v)) (not (is-non-tvar-matching? v))))

(define (same-matching-type? t1 t2)
  (or (and (is-tvar-matching? t1) (is-tvar-matching? t2))
      (and (is-non-tvar-matching? t1) (is-non-tvar-matching? t2))
      (and (is-general-matching? t1) (is-general-matching? t2))))

(define (can-match-tvar? t)
  (letrec ([f (λ (tprime)
                (cond [(term-variable? tprime) (is-tvar-matching? tprime)]
                      [(term-constant? tprime) #f]
                      [else (ormap f (sigma-term-term-list tprime))]))])
    (f t)))

(define (can-match-non-tvar? t)
  (letrec ([f (λ (tprime)
                (cond [(term-variable? tprime) (is-non-tvar-matching? tprime)]
                      [(term-constant? tprime) #t]
                      [else (andmap f (sigma-term-term-list tprime))]))])
    (f t)))

(define (can-match-var-to-term? var t)
  (cond [(is-tvar-matching? var) (can-match-tvar? t)]
        [(is-non-tvar-matching? var) (can-match-non-tvar? t)]
        [else #t]))

;; assumes varsolver rules variable naming conventions
(define (rename-to-tarvar-aware-vars term varmap [prefixes (list "tvar" "nvar" "v")])
  (letrec ([get-fresh-var (λ (var prefix)
                            (unless (hash-has-key? varmap var)
                                (hash-set! varmap var (format "~a~a" prefix (hash-count varmap))))
                            (hash-ref varmap var))]
           [f (λ (tprime)
                (cond [(and (term-variable? tprime)
                            (is-tvar-matching? tprime)) (get-fresh-var tprime (list-ref prefixes 0))]
                      [(and (term-variable? tprime)
                            (is-non-tvar-matching? tprime)) (get-fresh-var tprime (list-ref prefixes 1))]
                      [(and (term-variable? tprime)
                            (is-general-matching? tprime) (get-fresh-var tprime (list-ref prefixes 2)))]
                      [(term-constant? tprime) tprime]
                      [else (sigma-term (sigma-term-symbol tprime) (map f (sigma-term-term-list tprime)))]))])
    (cons varmap (f term))))

(define (rename-to-tarvar-aware-term-pairs terms varmap [prefixes (list "tvar" "nvar" "v")])
  (let* ([map-term-pair-a (rename-to-tarvar-aware-vars (car terms) varmap prefixes)]
         [map-term-pair-b (rename-to-tarvar-aware-vars (cdr terms) (car map-term-pair-a) prefixes)])
    (cons (cdr map-term-pair-a) (cdr map-term-pair-b))))

(define (term-size input-term)
  (letrec ([f (λ (t)
                (if (or (term-variable? t) (term-constant? t))
                    1
                    (foldl + 1 (map f (sigma-term-term-list t)))))])
    (f input-term)))

(define (term-op-count input-term)
  (letrec ([f (λ (t)
                (if (or (term-variable? t) (term-constant? t))
                    0
                    (foldl + 1 (map f (sigma-term-term-list t)))))])
    (f input-term)))

(define (cap-and-sort-terms-by-size n terms)
  (let ([term-size-pairs (filter (λ (p) (< (car p) n)) (map (λ (t) (cons (term-size t) t)) terms))])
    (map cdr (sort term-size-pairs (λ (p1 p2) (< (car p1) (car p2)))))))

(define (contains-target-variable? term var)
  (letrec ([f (λ (t)
                (cond [(term-variable? t) (equal? t var)]
                      [(term-constant? t) #f]
                      [else (ormap f (sigma-term-term-list t))]))])
    (f term)))

(define (contains-operator? term op)
  (letrec ([f (λ (t)
                (cond [(or (term-variable? t) (term-constant? t)) #f]
                      [(equal? (sigma-term-symbol t) op) #t]
                      [else (ormap f (sigma-term-term-list t))]))])
    (f term)))

;; true if expr typechecks when all vars are integer types, false otherwise
;; checks that no variables should be boolean type
;; assumes that halide expression would typecheck
(define (termIR->typechecks? term)
  (letrec ([f (λ (t)
                (cond [(or (term-variable? t) (term-constant? t)) #t]
                      [(and (equal? (sigma-term-symbol t) 'not)
                            (term-variable? (first (sigma-term-term-list t)))) #f]
                      [(and (equal? (sigma-term-symbol t) 'and)
                            (or (term-variable? (first (sigma-term-term-list t)))
                                (term-variable? (second (sigma-term-term-list t))))) #f]
                      [(and (equal? (sigma-term-symbol t) 'or)
                            (or (term-variable? (first (sigma-term-term-list t)))
                                (term-variable? (second (sigma-term-term-list t))))) #f]
                      [(and (equal? (sigma-term-symbol t) 'select)
                            (term-variable? (first (sigma-term-term-list t)))) #f]
                      [else (andmap f (sigma-term-term-list t))]))])
    (f term)))
