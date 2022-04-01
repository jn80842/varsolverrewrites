#lang rosette

(require "../traat/termIR.rkt")
(require "../typed-halide-lang.rkt")
(require "../typed-fixed-sketch-synthesis.rkt")
(require rackunit)

(current-bitwidth #f)

(check-equal? (interleave-arguments '(a b c) '(y z) '(0 2 3))
              '(a y b c z))

(check-true (fixed-sketch-obeys-order? (sigma-term '+ (list (sigma-term '+ (list "n0" "t0"))
                                                            (sigma-term '+ (list "t1" "n1"))))
                                       3op-patt1-metasketch
                                       '(0 2)))

(check-true (fixed-sketch-obeys-order? (sigma-term '+ (list (sigma-term '+ (list "n0" "t0"))
                                                            (sigma-term '+ (list "t0" "n1"))))
                                       3op-patt1-metasketch
                                       '(0 2)))

(check-equal? (fixed-sketch->termIR (fixed-sketch 2op-patt1-metasketch
                                                  operator-list
                                                  '(0 2)  '(1))
                                    (list "t0") (list "n0" "n1"))
              (sigma-term '+ (list "n0" (sigma-term '* (list "t0" "n1")))))

(check-equal? (eval-fixed-sketch (fixed-sketch 2op-patt1-metasketch
                                               operator-list
                                               '(0 2) '(1))
                                 (list 10) (list 3 2))
              23)

(check-equal? (sort-target-positions 3op-patt1-metasketch '("t0" "t1") '("n0" "n1") (list '(2 3) '(0 3) '(1 2) '(0 2)))
              (list '(0 2) '(0 3) '(1 2) '(2 3)))

(check-equal? (synthesize-from-fixed-metasketch (sigma-term '- (list (sigma-term '+ (list "n0" "n1")) "t0"))
                                                2op-patt1-metasketch)
              (sigma-term '- (list "n0" (sigma-term '- (list "t0" "n1")))))

(check-equal? (synthesize-from-fixed-metasketch (sigma-term '+ (list (sigma-term '- (list "n0" (sigma-term '- (list "t1" "n2")))) "t3"))
                                                3op-patt1-metasketch)
              (sigma-term '+ (list (sigma-term '- (list "n0" "t1")) (sigma-term '+ (list "t3" "n2")))))

(check-equal? (synthesize-from-fixed-metasketches (sigma-term '- (list (sigma-term '+ (list "n0" 3)) "t0")))
              (make-rule (sigma-term '- (list (sigma-term '+ (list "n0" 3)) "t0"))
                         (sigma-term '- (list "n0" (sigma-term '- (list "t0" 3))))))