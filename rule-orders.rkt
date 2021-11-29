#lang racket

(require "halide-parser.rkt")
(require "traat/termIR.rkt")
(require "traat/matching.rkt")
(require "traat/criticalpairs.rkt")
(require "TRSs/variablesolverTRS.rkt")
(require "TRSs/coqaxiomTRS.rkt")

(provide normalize normalize->termIR
         varsolver-reduction-order?
         tvar-count-reduction-order? tvar-count-reduction-order-equal?
         move-tvar-left-reduction-order? move-tvar-left-reduction-order-equal?
         move-tvar-up-reduction-order? move-tvar-up-reduction-order-equal? rule->halide-string benchmark-TRS)

(define targetedTRS
  (let ([rulelist (list (list "n0 + t0" "t0 + n0" "vsadd133")
                        (list "min(t0, t0)" "t0" "vsmin473")
                        (list "max(t0, t0)" "t0" "vsmax473")
                        (list "(t0 + n1) - t1" "(t0 - t1) + n1" "vssub267")
                        (list "t0 - (t1 + n1)" "(t0 - t1) - n1" "vssub270")
                        (list "(t0 * x) - (t0 * y)" "t0 * (x - y)" "vssub279")
                        (list "(t0 - n0) + n1" "t0 + (n1 - n0)" "vsadd152"))])
    (for/list ([r rulelist])
      (rule (halide->termIR (first r)) (halide->termIR (second r)) (third r)))))

(define (normalize input-halidestr tvar)
  (termIR->halide (varsolver-rewrite* tvar originalvarsolverTRS (halide->termIR input-halidestr) rule->halide-string)))

(define (normalize->termIR input-halidestr tvar)
  (varsolver-rewrite* tvar originalvarsolverTRS (halide->termIR input-halidestr)))

;; order checking
(define (count-target-variables r [target-matching? is-tvar-matching?])
  (letrec ([f (λ (t)
                (cond [(and (term-variable? t)
                            (target-matching? t)) 1]
                      [(and (term-variable? t)
                            (not (target-matching? t))) 0]
                      [(term-constant? t) 0]
                      [else (foldr + 0 (map f (sigma-term-term-list t)))]))])
    (f r)))

(define (tvar-count-reduction-order? r)
  (> (count-target-variables (rule-lhs r)) (count-target-variables (rule-rhs r))))

(define (tvar-count-reduction-order-equal? r)
  (equal? (count-target-variables (rule-lhs r)) (count-target-variables (rule-rhs r))))

(define (get-dps-ordered-variables t)
  (letrec ([f (λ (t)
                (cond [(term-variable? t) (list t)]
                      [(term-constant? t) (list "b")]
                      [(sigma-term? t) (map f (sigma-term-term-list t))]
                      [else '()]))])
    (flatten (f t))))

(define (move-tvar-left-reduction-order? r [target-matching? is-tvar-matching?])
  (let ([vars-lhs (get-dps-ordered-variables (rule-lhs r))]
        [vars-rhs (get-dps-ordered-variables (rule-rhs r))]
        [get-var-string (λ (l) (string-trim (string-join (map (λ (s) (if (target-matching? s) "a" "b")) l) "")
                                            "b" #:left? #f #:repeat? #t))])
    (and (equal? (filter target-matching? vars-lhs) (filter target-matching? vars-rhs))
         (string<? (get-var-string vars-rhs) (get-var-string vars-lhs)))))

(define (move-tvar-left-reduction-order-equal? r [target-matching? is-tvar-matching?])
  (let ([vars-lhs (get-dps-ordered-variables (rule-lhs r))]
        [vars-rhs (get-dps-ordered-variables (rule-rhs r))]
        [get-var-string (λ (l) (string-trim (string-join (map (λ (s) (if (target-matching? s) "a" "b")) l) "")
                                            "b" #:left? #f #:repeat? #t))])
    (and (equal? (filter target-matching? vars-lhs) (filter target-matching? vars-rhs))
         (equal? (get-var-string vars-rhs) (get-var-string vars-lhs)))))

(define (get-bfs-tvarcount-hash t [target-matching? is-tvar-matching?])
  (let ([varcount-hash (make-hash '())])
    (letrec ([f (λ (t n)
                  (cond [(and (term-variable? t) (target-matching? t)) (hash-set! varcount-hash n (add1 (hash-ref varcount-hash n 0)))]
                        [(and (term-variable? t) (not (target-matching? t))) (void)]
                        [(term-constant? t) (void)]
                        [(sigma-term? t) (map (λ (t1) (f t1 (add1 n))) (sigma-term-term-list t))]
                        [else (void)]))])
      (begin
        (f t 0)
        varcount-hash))))

(define (move-tvar-up-reduction-order? r)
  (let* ([lhs-hash (get-bfs-tvarcount-hash (rule-lhs r))]
         [rhs-hash (get-bfs-tvarcount-hash (rule-rhs r))]
         [all-keys (sort (remove-duplicates (append (hash-keys lhs-hash) (hash-keys rhs-hash))) <)])
    (for/first ([k all-keys]
      #:when (not (equal? (hash-ref lhs-hash k 0) (hash-ref rhs-hash k 0))))
      (< (hash-ref lhs-hash k 0) (hash-ref rhs-hash k 0)))))

(define (move-tvar-up-reduction-order-equal? r)
  (let ([lhs-hash (get-bfs-tvarcount-hash (rule-lhs r))]
        [rhs-hash (get-bfs-tvarcount-hash (rule-rhs r))])
    (equal? lhs-hash rhs-hash)))

(define (terms-reduction-order? target-var t1 t2)
  (let* ([is-target-variable? (λ (v) (equal? v target-var))]
         [t1-target-var-count (count-target-variables t1 is-target-variable?)]
         [t2-target-var-count (count-target-variables t2 is-target-variable?)]
         [dummy-rule (rule t1 t2 "")]
         [t1-bfs-var-hash (get-bfs-tvarcount-hash t1 is-target-variable?)]
         [t2-bfs-var-hash (get-bfs-tvarcount-hash t2 is-target-variable?)])
    (or (> t1-target-var-count t2-target-var-count)
        (and (equal? t1-target-var-count t2-target-var-count)
             (move-tvar-left-reduction-order? dummy-rule is-target-variable?))
        (and (equal? t1-target-var-count t2-target-var-count)
             (move-tvar-left-reduction-order-equal? dummy-rule is-target-variable?)
             (for/first ([k (sort (remove-duplicates (append (hash-keys t1-bfs-var-hash) (hash-keys t2-bfs-var-hash))) <)]
                         #:when (not (equal? (hash-ref t1-bfs-var-hash k 0) (hash-ref t2-bfs-var-hash k 0))))
               (< (hash-ref t1-bfs-var-hash k 0) (hash-ref t2-bfs-var-hash k 0)))))))

(define (varsolver-reduction-order? r)
  (or (tvar-count-reduction-order? r)
      (and (tvar-count-reduction-order-equal? r) (move-tvar-left-reduction-order? r))
      (and (tvar-count-reduction-order-equal? r) (move-tvar-left-reduction-order-equal? r) (move-tvar-up-reduction-order? r))))

(define (varsolver-reduction-order2? r)
  (or (tvar-count-reduction-order? r)
      (and (tvar-count-reduction-order-equal? r) (move-tvar-up-reduction-order? r))
      (and (tvar-count-reduction-order-equal? r) (move-tvar-up-reduction-order-equal? r) (move-tvar-up-reduction-order? r))))

(define (check-ruleset-reduction-order TRS)
  (λ (check-func)
    (for ([r TRS])
      (unless (check-func r)
        (displayln (format "Failed reduction order: ~a -> ~a (~a)"
                           (termIR->halide (rule-lhs r))
                           (termIR->halide (rule-rhs r))
                           (rule-name r)))))))

(define originalvarsolver-orders (make-hash '()))
(for ([r originalvarsolverTRS])
    (hash-set! originalvarsolver-orders (rule-name r) '())
    (unless (not (tvar-count-reduction-order? r))
      (hash-set! originalvarsolver-orders (rule-name r) (cons 'count (hash-ref originalvarsolver-orders (rule-name r)))))
    (unless (not (move-tvar-up-reduction-order? r))
      (hash-set! originalvarsolver-orders (rule-name r) (cons 'up (hash-ref originalvarsolver-orders (rule-name r)))))
    (unless (not (move-tvar-left-reduction-order? r))
      (hash-set! originalvarsolver-orders (rule-name r) (cons 'left (hash-ref originalvarsolver-orders (rule-name r))))))

(define varcount-reducing-TRS
  (filter tvar-count-reduction-order? originalvarsolverTRS))

(define movevarup-TRS
  (filter move-tvar-up-reduction-order? originalvarsolverTRS))

(define movevarleft-TRS
  (filter move-tvar-left-reduction-order? originalvarsolverTRS))

(define fewerandup-TRS
  (filter (λ (r) (or (tvar-count-reduction-order? r)
                     (move-tvar-up-reduction-order? r))) originalvarsolverTRS))

(define fewerandleft-TRS
  (filter (λ (r) (or (tvar-count-reduction-order? r)
                     (move-tvar-left-reduction-order? r))) originalvarsolverTRS))

(define upandleft-TRS
  (filter (λ (r) (or (move-tvar-up-reduction-order? r)
                     (move-tvar-left-reduction-order? r))) originalvarsolverTRS))

(define (rule->halide-string r)
  (let ([order-string (string-join (map symbol->string (hash-ref originalvarsolver-orders (rule-name r) (list 'unknownorder))))])
    (if (non-empty-string? (rule-name r))
        (format "~a -> ~a (~a) [~a]" (termIR->halide (rule-lhs r)) (termIR->halide (rule-rhs r)) (rule-name r) order-string)
        (format "~a -> ~a [~a]" (termIR->halide (rule-lhs r)) (termIR->halide (rule-rhs r)) order-string))))

(define (benchmark-TRS TRS)
  (with-input-from-file "benchmarks.txt"
                  (thunk
                   (define solved-count 0)
                    (for ([e (in-lines)])
                      (begin
                        (displayln (format "INPUT: ~a" e))
                       (let ([normalizedIR (varsolver-rewrite* "x" TRS (halide->termIR e) rule->halide-string)])
                        (if (termIR->in-solved-form? normalizedIR "x")
                            (begin (displayln (format "SOLVED: ~a to ~a" e (termIR->halide normalizedIR)))
                                   (set! solved-count (add1 solved-count)))
                            (displayln (format "NOT SOLVED: ~a to ~a" e (termIR->halide normalizedIR))))))
                    (displayln (format "benchmarks solved: ~a" solved-count))))))

(define (benchmark-and-save-to-files TRS solved-out-file not-solved-out-file)
  (define solved-out (open-output-file solved-out-file))
  (define not-solved-out (open-output-file not-solved-out-file))
  (with-input-from-file "benchmarks.txt"
    (thunk
     (for ([e (in-lines)])
       (let ([normalized-term (varsolver-rewrite* "x" TRS (halide->termIR e) rule->halide-string)])
         (if (termIR->in-solved-form? normalized-term "x")
             (displayln e solved-out)
             (displayln e not-solved-out))))))
  (close-output-port solved-out)
  (close-output-port not-solved-out))

(define (benchmark-set-difference TRS1 TRS2 solved1-file solved2-file)
  (define solved1-out (open-output-file solved1-file))
  (define solved2-out (open-output-file solved2-file))
  (with-input-from-file "benchmarks.txt"
    (thunk
     (for ([e (in-lines)])
       (let ([normalized-term1 (varsolver-rewrite* "x" TRS1 (halide->termIR e) rule->halide-string)]
             [normalized-term2 (varsolver-rewrite* "x" TRS2 (halide->termIR e) rule->halide-string)])
         (if (and (termIR->in-solved-form? normalized-term1 "x")
                  (not (termIR->in-solved-form? normalized-term2 "x")))
             (displayln e solved1-out)
             (if (and (not (termIR->in-solved-form? normalized-term1 "x"))
                      (termIR->in-solved-form? normalized-term2 "x"))
                 (displayln e solved2-out)
                 (void)))))))
  (close-output-port solved1-out)
  (close-output-port solved2-out))

(define (normalize-coq halide-term)
  (termIR->halide (varsolver-rewrite* "x" coqaxiomTRS (halide->termIR halide-term) rule->halide-string)))

(define (normalize-coqvs halide-term)
  (termIR->halide (varsolver-rewrite* "x" (append coqaxiomTRS originalvarsolverTRS) (halide->termIR halide-term) rule->halide-string)))

(define (normalize-vscoq halide-term)
  (termIR->halide (varsolver-rewrite* "x" (append originalvarsolverTRS coqaxiomTRS) (halide->termIR halide-term) rule->halide-string)))
;;(benchmark-TRS (list (make-rule (halide->termIR "(t0 - t0)") 0) (make-rule (halide->termIR "(t0 >= t0)") 'true)))

(define expr1 "((((x*16) + ((y/8)*8)) + -20) <= ((((y + 103)/8)*8) + -12))")

(define completion-rules
  (list (rule (halide->termIR "(t0 - (t1 - n0)) - t2")
              (halide->termIR "((t0 - t1) - t2) + n0")
              "completionrule1")))

;; candidates for synthesizing missing rule
;;  (max(min(likely(x), 13), 0) - max(min(x, 13), 0))
;;  !(x < (max(y, 1)/96))
;; (((((y*16) + ((z*16) + (x*32))) + w) + -15) <= ((x*32) + (z*16)))
;;(((x * 32) + ((((z * 16) + (y * 16)) + w) + -15)) <= ((x * 32) + (z * 16)))

(define fixedTRS
(list
 (rule (sigma-term '< '("n0" "t1")) (sigma-term '> '("t1" "n0")) "R1")
 (rule (sigma-term '<= '("n0" "t1")) (sigma-term '>= '("t1" "n0")) "R2")
 (rule (sigma-term '>= '("n0" "t1")) (sigma-term '<= '("t1" "n0")) "R3")
 (rule
  (sigma-term '! (list (sigma-term '> '("t0" "n1"))))
  (sigma-term '<= '("t0" "n1"))
  "R4")
 (rule
  (sigma-term '! (list (sigma-term '< '("t0" "n1"))))
  (sigma-term '>= '("t0" "n1"))
  "R5")
 (rule
  (sigma-term '! (list (sigma-term '>= '("t0" "n1"))))
  (sigma-term '< '("t0" "n1"))
  "R6")
 (rule
  (sigma-term '! (list (sigma-term '== '("t0" "n1"))))
  (sigma-term '!= '("t0" "n1"))
  "R7")
 (rule
  (sigma-term '<= (list (sigma-term '+ '("t0" "n1")) "n2"))
  (sigma-term '<= (list "t0" (sigma-term '- '("n2" "n1"))))
  "R8")
 (rule
  (sigma-term '>= (list (sigma-term '+ '("t0" "n1")) "n2"))
  (sigma-term '>= (list "t0" (sigma-term '- '("n2" "n1"))))
  "R9")
 (rule
  (sigma-term '<= (list (sigma-term '- '("t0" "n1")) "n2"))
  (sigma-term '<= (list "t0" (sigma-term '+ '("n2" "n1"))))
  "R10")
 (rule
  (sigma-term '>= (list (sigma-term '- '("t0" "n1")) "n2"))
  (sigma-term '>= (list "t0" (sigma-term '+ '("n2" "n1"))))
  "R11")
 (rule
  (sigma-term 'min '("n0" "t1"))
  (sigma-term 'min '("t1" "n0"))
  "R12")
 (rule
  (sigma-term '< (list (sigma-term '+ '("t0" "n1")) "n2"))
  (sigma-term '< (list "t0" (sigma-term '- '("n2" "n1"))))
  "R13")
 (rule (sigma-term '+ '("n0" "t1")) (sigma-term '+ '("t1" "n0")) "R14")
 (rule
  (sigma-term
   '+
   (list (sigma-term '* '("t0" "n1")) (sigma-term '* '("t0" "n2"))))
  (sigma-term '* (list "t0" (sigma-term '+ '("n1" "n2"))))
  "R15")
 (rule
  (sigma-term
   '<=
   (list (sigma-term '* '("t0" "n1")) (sigma-term '* '("t0" "n1"))))
  (sigma-term '<= '("n1" "n1"))
  "R16")
 (rule
  (sigma-term '<= (list (sigma-term '- '("n0" "t1")) "n2"))
  (sigma-term '>= (list "t1" (sigma-term '- '("n0" "n2"))))
  "R17")
 (rule
  (sigma-term '>= (list (sigma-term '- '("n0" "t1")) "n2"))
  (sigma-term '<= (list "t1" (sigma-term '- '("n0" "n2"))))
  "R18")
 (rule
  (sigma-term '- (list (sigma-term '+ '("t0" "n1")) "n2"))
  (sigma-term '+ (list "t0" (sigma-term '- '("n1" "n2"))))
  "R19")
 (rule
  (sigma-term '+ (list (sigma-term '+ '("t0" "n1")) "n2"))
  (sigma-term '+ (list "t0" (sigma-term '+ '("n1" "n2"))))
  "R20")
 (rule
  (sigma-term '+ (list (sigma-term '- '("t0" "n1")) "n2"))
  (sigma-term '- (list "t0" (sigma-term '- '("n1" "n2"))))
  "R21")
 (rule
  (sigma-term
   '+
   (list
    (sigma-term '* '("t0" "n1"))
    (sigma-term '+ (list (sigma-term '* '("t0" "n2")) "n3"))))
  (sigma-term
   '+
   (list "n3" (sigma-term '* (list "t0" (sigma-term '+ '("n2" "n1"))))))
  "R22")
 (rule
  (sigma-term '<= (list "t0" (sigma-term '+ '("t0" "n1"))))
  (sigma-term '>= (list "n1" (sigma-term '- '("n1" "n1"))))
  "R23")
 (rule
  (sigma-term
   '-
   (list
    (sigma-term
     'min
     (list (sigma-term 'max '("t0" "n1")) (sigma-term '+ '("t0" "n2"))))
    "t0"))
  (sigma-term
   'min
   (list
    "n2"
    (sigma-term '- (list "n1" (sigma-term 'min '("t0" "n1"))))))
  "R24")
 (rule
  (sigma-term
   '-
   (list
    (sigma-term
     '+
     (list
      (sigma-term '* (list (sigma-term '+ '("t0" "n1")) "n2"))
      "n3"))
    (sigma-term '* '("t0" "n2"))))
  (sigma-term '+ (list (sigma-term '* '("n1" "n2")) "n3"))
  "R25")
 (rule
  (sigma-term '> (list (sigma-term '+ '("t0" "n1")) "n2"))
  (sigma-term '> (list "t0" (sigma-term '- '("n2" "n1"))))
  "R26")
 (rule
  (sigma-term '- (list (sigma-term '+ '("t0" "n1")) "t0"))
  "n1"
  "R27")
 (rule
  (sigma-term '- (list "t0" (sigma-term '+ '("t0" "n1"))))
  (sigma-term '- (list "n1" (sigma-term '+ '("n1" "n1"))))
  "R28")
 (rule
  (sigma-term
   '-
   (list
    (sigma-term
     'select
     (list
      (sigma-term '< '("n0" "n1"))
      (sigma-term '+ '("t2" "n3"))
      "t2"))
    "t2"))
  (sigma-term
   'select
   (list (sigma-term '> '("n1" "n0")) "n3" (sigma-term '- '("n3" "n3"))))
  "R29")
 (rule
  (sigma-term
   '+
   (list
    "t0"
    (sigma-term 'min (list (sigma-term '- '("n1" "t0")) "n2"))))
  (sigma-term 'min (list "n1" (sigma-term '+ '("t0" "n2"))))
  "R30")
 (rule
  (sigma-term
   '-
   (list
    (sigma-term 'min (list (sigma-term '+ '("t0" "n1")) "n2"))
    "t0"))
  (sigma-term 'min (list "n1" (sigma-term '- '("n2" "t0"))))
  "R31")
 (rule
  (sigma-term
   '-
   (list (sigma-term 'min '("t0" "n1")) (sigma-term 'min '("t0" "n1"))))
  (sigma-term '- '("n1" "n1"))
  "R32")
 (rule
  (sigma-term
   '-
   (list (sigma-term '+ '("t0" "n1")) (sigma-term '+ '("t0" "n2"))))
  (sigma-term '- '("n1" "n2"))
  "R33")
 (rule
  (sigma-term
   '<=
   (list (sigma-term '+ '("t0" "n1")) (sigma-term '+ '("t0" "n2"))))
  (sigma-term '>= '("n2" "n1"))
  "R34")))

#;(time (with-input-from-file "testset.txt"
  (λ () (for/fold ([handwritten-count 0]
                   [synthed-count 0]
                   [hgreaters 0]
                   [sgreaterh 0])
                   ([l (in-lines)])
          (let ([handwritten-t (varsolver-rewrite* "x" variablesolverTRS (halide->termIR (string-trim l "\"")))]
                [synthed-t (varsolver-rewrite* "x" fixedTRS (halide->termIR (string-trim l "\"")))])
                  (values (+ handwritten-count
                             (if (termIR->in-solved-form? handwritten-t "x") 1 0))
                          (+ synthed-count (if (termIR->in-solved-form? synthed-t "x") 1 0))
                          (+ hgreaters (if (terms-reduction-order? "x" handwritten-t synthed-t) 1 0))
                          (+ sgreaterh (if (terms-reduction-order? "x" synthed-t handwritten-t) 1 0))
                          ))))))