#lang rosette

(require rackunit)
(require "../typed-halide-lang.rkt")

(define 10int (register 'int (bv 10 8)))
(define 4int (register 'int (bv 4 8)))
(define 3int (register 'int (bv 3 8)))
(define 2int (register 'int (bv 2 8)))
(define 1int (register 'int (bv 1 8)))
(define n10int (register 'int (bv -10 8)))
(define n4int (register 'int (bv -4 8)))
(define n3int (register 'int (bv -3 8)))
(define n2int (register 'int (bv -2 8)))
(define n1int (register 'int (bv -1 8)))

(if USEINT
    (begin

(check-equal? (hld-add (register 'int 2) (register 'int 5)) (register 'int 7))

      )
    (begin
(check-equal? (hld-div 10int 3int) 3int)
(check-equal? (hld-div 10int n3int) n3int)
(check-equal? (hld-div n10int 3int) n4int)
(check-equal? (hld-div n10int n3int) 4int)

(check-equal? (hld-mod 10int 3int) 1int)
(check-equal? (hld-mod 10int n3int) 1int)
(check-equal? (hld-mod n10int 3int) 2int)
(check-equal? (hld-mod n10int n3int) 2int)
))

