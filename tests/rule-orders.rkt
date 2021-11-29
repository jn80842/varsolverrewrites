#lang racket

(require rackunit)
(require "../traat/termIR.rkt")
(require "../traat/matching.rkt")
(require "../halide-parser.rkt")
(require "../rule-orders.rkt")

(define vssub279-rule (rule (halide->termIR "(t0 * x) - (t0 * y)")
                            (halide->termIR "t0 * (x - y)") "vssub279"))
(define vsadd155-rule (rule (halide->termIR "(t0 + n0) + n1")
                            (halide->termIR "t0 + (n0 + n1)") "vsadd155"))
(define vsor588-rule (rule (halide->termIR "t0 || (t1 || n0)")
                           (halide->termIR "(t0 || t1) || n0") "vsor588"))
(define vssub282-rule (rule (halide->termIR "(t0 * x) - (t1 * x)")
                            (halide->termIR "(t0 - t1) * x") "vssub282"))

(check-true (tvar-count-reduction-order? vssub279-rule))
(check-false (tvar-count-reduction-order? vsadd155-rule))

(check-true (tvar-count-reduction-order-equal? vsadd155-rule))

(check-true (move-tvar-left-reduction-order? vssub282-rule))
(check-false (move-tvar-left-reduction-order? vsor588-rule))
(check-true (move-tvar-left-reduction-order-equal? vsor588-rule))

(check-true (move-tvar-up-reduction-order? vssub279-rule))
(check-false (move-tvar-up-reduction-order? vssub282-rule))
(check-true (move-tvar-up-reduction-order-equal? vssub282-rule))