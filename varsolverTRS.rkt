#lang racket

(require "halide-parser.rkt")
(require "trat/termIR.rkt")
(require "trat/matching.rkt")

(provide originalvarsolverTRS normalize normalize->termIR
         tvar-count-reduction-order? tvar-count-reduction-order-equal?
         move-tvar-left-reduction-order? move-tvar-left-reduction-order-equal?
         move-tvar-up-reduction-order? move-tvar-up-reduction-order-equal?)

;; commented out original rules that don't obey reduction order
(define originalvarsolverTRS-halide (list
(list "n0 + t0" "t0 + n0" "vsadd133")
(list "(t0 - n0) + n1" "t0 + (n1 - n0)" "vsadd152")
(list "(t0 + n0) + n1" "t0 + (n0 + n1)" "vsadd155")
(list "t0 + t0" "t0 * 2" "vsadd159")
(list "(t0 + n0) + t1" "(t0 + t1) + n0" "vsadd162")
;;(list "t0 + (t1 + n0)" "(t0 + t1) + n0" "vsadd165")
(list "(t0 - n0) + t1" "(t0 + t1) - n0" "vsadd168")
;;(list "t0 + (t1 - n0)" "(t0 + t1) - n0" "vsadd171")
(list "(t0 * x) + (t0 * y)" "t0 * (x + y)" "vsadd174")
(list "(t0 * x) + (t1 * x)" "(t0 + t1) * x" "vsadd177")
(list "(t0 * x) + t0" "t0 * (x + 1)" "vsadd180")
(list "t0 + (t0 * x)" "t0 * (x + 1)" "vsadd183")
(list "(t0 - n1) - n0" "t0 - (n1 + n0)" "vssub240")
(list "(t0 + n1) - n0" "t0 + (n1 - n0)" "vssub243")
(list "n0 - (t0 - n1)" "(t0 * -1) + (n0 + n1)" "vssub257")
(list "n0 - (t0 + n1)" "(t0 * -1) + (n0 - n1)" "vssub260")
(list "n0 - t0" "(t0 * -1) + n0" "vssub262")
(list "(t0 + n1) - t1" "(t0 - t1) + n1" "vssub267")
;;(list "t0 - (t1 + n1)" "(t0 - t1) - n1" "vssub270")
(list "(t0 - n1) - t1" "(t0 - t1) - n1" "vssub273")
;;(list "t0 - (t1 - n1)" "(t0 - t1) + n1" "vssub276")
(list "(t0 * x) - (t0 * y)" "t0 * (x - y)" "vssub279")
(list "(t0 * x) - (t1 * x)" "(t0 - t1) * x" "vssub282")
(list "n0 * t0" "t0 * n0" "vsmul326")
;;(list "(t0 + n0) * n1" "t0 * n1 + n0 * n1" "vsmul339")
;;(list "(t0 - n0) * n1" "t0 * n1 - n0 * n1" "vsmul342")
(list "(t0 * n0) * n1" "t0 * (n0 * n1)" "vsmul345")
(list "max(n0, t0)" "max(t0, n0)" "vsmax449")
(list "max(t0, t0)" "t0" "vsmax473")
(list "max(max(t0, n0), t1)" "max(max(t0, t1), n0)" "vsmax476")
;;(list "max(t0, max(t1, n0))" "max(max(t0, t1), n0)" "vsmax479")
(list "max(t0 + n0, t0 + n1)" "t0 + max(n0, n1)" "vsmax482")
(list "max(t0 + n0, t1 + n0)" "max(t0, t1) + n0" "vsmax485")
(list "max(t0 + n0, t0)" "t0 + max(n0, 0)" "vsmax488")
(list "max(t0, t0 + n0)" "t0 + max(n0, 0)" "vsmax491")
(list "max(t0 - n0, t1 - n0)" "max(t0, t1) - n0" "vsmax503")
(list "max(t0 - n0, t0)" "t0 - min(n0, 0)" "vsmax506")
(list "max(t0, t0 - n0)" "t0 - min(n0, 0)" "vsmax509")
(list "min(n0, t0)" "min(t0, n0)" "vsmin449")
(list "min(t0, t0)" "t0" "vsmin473")
(list "min(min(t0, n0), t1)" "min(min(t0, t1), n0)" "vsmin476")
;;(list "min(t0, min(t1, n0))" "min(min(t0, t1), n0)" "vsmin479")
(list "min(t0 + n0, t0 + n1)" "t0 + min(n0, n1)" "vsmin482")
(list "min(t0 + n0, t1 + n0)" "min(t0, t1) + n0" "vsmin485")
(list "min(t0 + n0, t0)" "t0 + min(n0, 0)" "vsmin488")
(list "min(t0, t0 + n0)" "t0 + min(n0, 0)" "vsmin491")
(list "min(t0 - n0, t0 - n1)" "t0 - max(n0, n1)" "vsmin494")
(list "min(t0 - n0, t0 + n1)" "t0 + min(0 - n0, n1)" "vsmin497")
(list "min(t0 + n0, t0 - n1)" "t0 + min(n0, 0 - n1)" "vsmin500")
(list "min(t0 - n0, t1 - n0)" "min(t0, t1) - n0" "vsmin503")
(list "min(t0 - n0, t0)" "t0 - max(n0, 0)" "vsmin506")
(list "min(t0, t0 - n0)" "t0 - max(n0, 0)" "vsmin509")
(list "n0 && t0" "t0 && n0" "vsand564")
(list "t0 && t0" "t0" "vsand582")
(list "(t0 && n0) && t1" "(t0 && t1) && n0" "vsand585")
;;(list "t0 && (t1 && n0)" "(t0 && t1) && n0" "vsand588")
(list "n0 || t0" "t0 || n0" "vsor564")
(list "t0 || t0" "t0" "vsor582")
(list "(t0 || n0) || t1" "(t0 || t1) || n0" "vsor585")
(list "t0 || (t1 || n0)" "(t0 || t1) || n0" "vsor588")
(list "n0 == t0" "t0 == n0" "vseq636")
(list "(t0 + n1) == n0" "t0 == (n0 - n1)" "vseq657")
(list "(t0 - n1) == n0" "t0 == (n1 + n0)" "vseq660")
(list "(t0 * -1) == n0" "t0 == (0 - n0)" "vseq670")
;;(list "t0 == t1" "(t0 - t1) == 0" "vseq737")
(list "n0 != t0" "t0 != n0" "vsne636")
(list "(t0 + n1) != n0" "t0 != (n0 - n1)" "vsne657")
(list "(t0 - n1) != n0" "t0 != (n1 + n0)" "vsne660")
(list "(t0 * -1) != n0" "t0 != (0 - n0)" "vsne670")
;;(list "t0 != t1" "(t0 - t1) != 0" "vsne737")
(list "n0 < t0" "t0 > n0" "vslt636")
(list "(t0 + n1) < n0" "t0 < (n0 - n1)" "vslt657")
(list "(t0 - n1) < n0" "t0 < (n1 + n0)" "vslt660")
(list "(t0 * -1) < n0" "t0 > (0 - n0)" "vslt670")
;;(list "t0 < t1" "(t0 - t1) < 0" "vslt737")
(list "n0 <= t0" "t0 >= n0" "vsle636")
(list "(t0 + n1) <= n0" "t0 <= (n0 - n1)" "vsle657")
(list "(t0 - n1) <= n0" "t0 <= (n1 + n0)" "vsle660")
(list "(t0 * -1) <= n0" "t0 >= (0 - n0)" "vsle670")
;;(list "t0 <= t1" "(t0 - t1) <= 0" "vsle737")
(list "n0 > t0" "t0 < n0" "vsgt636")
(list "(t0 + n1) > n0" "t0 > (n0 - n1)" "vsgt657")
(list "(t0 - n1) > n0" "t0 > (n1 + n0)" "vsgt660")
(list "(t0 * -1) > n0" "t0 < (0 - n0)" "vsgt670")
;;(list "t0 > t1" "(t0 - t1) > 0" "vsgt737")
(list "n0 >= t0" "t0 <= n0" "vsge636")
(list "(t0 + n1) >= n0" "t0 >= (n0 - n1)" "vsge657")
(list "(t0 - n1) >= n0" "t0 >= (n1 + n0)" "vsge660")
(list "(t0 * -1) >= n0" "t0 <= (0 - n0)" "vsge670")
;;(list "t0 >= t1" "(t0 - t1) >= 0" "vsge737")
))

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

(define originalvarsolverTRS
  (for/list ([r originalvarsolverTRS-halide])
    (rule (halide->termIR (first r)) (halide->termIR (second r)) (third r))))

(define (normalize input-halidestr tvar)
  (termIR->halide (varsolver-rewrite* tvar originalvarsolverTRS (halide->termIR input-halidestr) rule->halide-string)))

(define (normalize->termIR input-halidestr tvar)
  (varsolver-rewrite* tvar originalvarsolverTRS (halide->termIR input-halidestr)))

;; order checking
(define (count-target-variables r)
  (letrec ([f (λ (t)
                (cond [(and (term-variable? t)
                            (is-tvar-matching? t)) 1]
                      [(and (term-variable? t)
                            (not (is-tvar-matching? t))) 0]
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
                      [(term-constant? t) '()]
                      [(sigma-term? t) (map f (sigma-term-term-list t))]
                      [else '()]))])
    (flatten (f t))))

(define (move-tvar-left-reduction-order? r)
  (let ([vars-lhs (get-dps-ordered-variables (rule-lhs r))]
        [vars-rhs (get-dps-ordered-variables (rule-rhs r))]
        [get-var-string (λ (l) (string-join (map (λ (s) (if (is-tvar-matching? s) "a" "b")) l) ""))])
    (and (equal? (filter is-tvar-matching? vars-lhs) (filter is-tvar-matching? vars-rhs))
         (string<? (get-var-string vars-rhs) (get-var-string vars-lhs)))))

(define (move-tvar-left-reduction-order-equal? r)
  (let ([vars-lhs (get-dps-ordered-variables (rule-lhs r))]
        [vars-rhs (get-dps-ordered-variables (rule-rhs r))]
        [get-var-string (λ (l) (string-join (map (λ (s) (if (is-tvar-matching? s) "a" "b")) l) ""))])
    (and (equal? (filter is-tvar-matching? vars-lhs) (filter is-tvar-matching? vars-rhs))
         (equal? (get-var-string vars-rhs) (get-var-string vars-lhs)))))

(define (get-bfs-tvarcount-hash t)
  (let ([varcount-hash (make-hash '())])
    (letrec ([f (λ (t n)
                  (cond [(and (term-variable? t) (is-tvar-matching? t)) (hash-set! varcount-hash n (add1 (hash-ref varcount-hash n 0)))]
                        [(and (term-variable? t) (not (is-tvar-matching? t))) (void)]
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

(define (rule->halide-string r)
  (let ([order-string (string-join (map symbol->string (hash-ref originalvarsolver-orders (rule-name r))))])
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
                       (let ([normalizedIR (varsolver-rewrite* "x" TRS (halide->termIR e))])
                        (if (termIR->in-solved-form? normalizedIR "x")
                            (begin (displayln (format "SOLVED: ~a to ~a" e (termIR->halide normalizedIR)))
                                   (set! solved-count (add1 solved-count)))
                            (displayln (format "NOT SOLVED: ~a to ~a" e (termIR->halide normalizedIR))))))
                    (displayln (format "benchmarks solved: ~a" solved-count))))))