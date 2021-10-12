#lang racket

(require "halide-parser.rkt")
(require "traat/termIR.rkt")

(provide variablesolverTRS)

(define variablesolverTRS-halide
  (list
     (list "n0 + t0" "t0 + n0" "vsadd133")
  (list "(t0 - n0) + n1" "t0 + (n1 - n0)" "vsadd152")
  (list "(t0 + n0) + n1" "t0 + (n0 + n1)" "vsadd155")
  (list "t0 + t0" "t0 * 2" "vsadd159")
  (list "(t0 + n0) + t1" "(t0 + t1) + n0" "vsadd162")
         ;; BREAKS REDUCTION ORDER (t0 + (t1 + n0), (t0 + t1) + n0, "vsadd165")) || // ln 165 // stdlib
(list "(t0 - n0) + t1" "(t0 + t1) - n0" "vsadd168")
         ;; BREAKS REDUCTION ORDER (rewrite(t0 + (t1 - n0), (t0 + t1) - n0, "vsadd171")) || // ln 171
(list "(t0 * x) + (t0 * y)" "t0 * (x + y)" "vsadd174")
(list "(t0 * x) + (t1 * x)" "(t0 + t1) * x" "vsadd177")
(list "(t0 * x) + t0" "t0 * (x + 1)" "vsadd180")
(list "t0 + (t0 * x)" "t0 * (x + 1)" "vsadd183")
        ;; PREDICATE "(t0 / c0) + t1" "(t0 + (t1 * c0)) / c0, c0 != 0, "vsadd186")) || // ln 186 // reduction order????
        ;; PREDICATE "t0 + (x / c0)" "((t0 * c0) + x) / c0, c0 != 0, "vsadd189")) || // ln 189 // reduction order?????
(list "(t0 - t0)" "0" "missingrule1")
(list "(t0 - n1) - n0" "t0 - (n1 + n0)" "vssub240")
(list "(t0 + n1) - n0" "t0 + (n1 - n0)" "vssub243")
(list "n0 - (t0 - n1)" "(t0 * -1) + (n0 + n1)" "vssub257")
(list "n0 - (t0 + n1)" "(t0 * -1) + (n0 - n1)" "vssub260")
(list "n0 - t0, (t0 * -1) + n0" "vssub262")
(list "(t0 + n1) - t1" "(t0 - t1) + n1" "vssub267")
        ;; BREAKS REDUCTION ORDER (rewrite(t0 - (t1 + n1), (t0 - t1) - n1, "vssub270")) || // ln 270 // reduction order? // stdlib
(list "(t0 - n1) - t1" "(t0 - t1) - n1" "vssub273")
        ;; BREAKS REDUCTION ORDER (rewrite(t0 - (t1 - n1), (t0 - t1) + n1, "vssub276")) || // ln 276 // reduction order? // stdlib
(list "(t0 * x) - (t0 * y)" "t0 * (x - y)" "vssub279")
(list "(t0 * x) - (t1 * x)" "(t0 - t1) * x" "vssub282")
        ;; PREDICATE (list "(t0 / c0) - t1" "(t0 - (t1 * c0)) / c0, c0 != 0" "vssub285")) || // ln 285 // reduction order?
(list "n0 * t0" "t0 * n0" "vsmul326")
        ;; BREAKS REDUCTION ORDER (rewrite((t0 + n0) * n1, t0 * n1 + n0 * n1, "vsmul339")) || // ln 339
        ;; BREAKS REDUCTION ORDER (rewrite((t0 - n0) * n1, t0 * n1 - n0 * n1, "vsmul342")) || // ln 342
(list "(t0 * n0) * n1" "t0 * (n0 * n1)" "vsmul345")
        ;; PREDICATE (rewrite((t0 + n0) / n1, t0/n1 + n0/n1, can_prove(((t0/n1)*n1) == t0, this))) || // ln 394
        ;; PREDICATE (rewrite((t0 - n0) / n1, t0/n1 - n0/n1, can_prove(((t0/n1)*n1) == t0, this))) || // ln 398
        ;; PREDICATE (rewrite((t0 * n0) / n1, t0 * (n0/n1), can_prove(((t0/n1)*n1) == t0, this))) || // ln 402
(list "max(n0, t0)" "max(t0, n0)" "vsmax449")
(list "max(t0, t0)" "t0" "vsmax473")
(list "max(max(t0, n0), t1)" "max(max(t0, t1), n0)" "vsmax476")
        ;; BREAKS REDUCTION ORDER (rewrite(max(t0, max(t1, n0)), max(max(t0, t1), n0), "vsmax479")) || // ln 479
(list "max(t0 + n0, t0 + n1)" "t0 + max(n0, n1)" "vsmax482")
(list "max(t0 + n0, t1 + n0)" "max(t0, t1) + n0" "vsmax485")
(list "max(t0 + n0, t0)" "t0 + max(n0, 0)" "vsmax488")
(list "max(t0, t0 + n0)" "t0 + max(n0, 0)" "vsmax491")
(list "max(t0 - n0, t1 - n0)" "max(t0, t1) - n0" "vsmax503")
(list "max(t0 - n0, t0)" "t0 - min(n0, 0)" "vsmax506")
(list "max(t0, t0 - n0)" "t0 - min(n0, 0)" "vsmax509")
        ;; PREDICATE (list "max(t0 * c0, t1 * c0)" "max(t0, t1) * c0, c0 > 0" "vsmax513")) || // ln 513
(list "min(n0, t0)" "min(t0, n0)" "vsmin449")
(list "min(t0, t0)" "t0" "vsmin473")
(list "min(min(t0, n0)" "t1), min(min(t0, t1), n0)" "vsmin476")
        ;; BREAKS REDUCTION ORDER (rewrite(min(t0, min(t1, n0)), min(min(t0, t1), n0), "vsmin479")) || // ln 479
(list "min(t0 + n0, t0 + n1)" "t0 + min(n0, n1)" "vsmin482")
(list "min(t0 + n0, t1 + n0)" "min(t0, t1) + n0" "vsmin485")
(list "min(t0 + n0, t0)" "t0 + min(n0, 0)" "vsmin488")
(list "min(t0, t0 + n0)" "t0 + min(n0, 0)" "vsmin491")
(list "min(t0 - n0, t0 - n1)" "t0 - max(n0, n1)" "vsmin494")
(list "min(t0 - n0, t0 + n1)" "t0 + min(0 - n0, n1)" "vsmin497")
(list "min(t0 + n0, t0 - n1)" "t0 + min(n0, 0 - n1)" "vsmin500")
(list "min(t0 - n0, t1 - n0), min(t0, t1) - n0" "vsmin503")
(list "min(t0 - n0, t0)" "t0 - max(n0, 0)" "vsmin506")
(list "min(t0, t0 - n0)" "t0 - max(n0, 0)" "vsmin509")
        ;; PREDICATE (list "min(t0 * c0, t1 * c0)" "min(t0, t1) * c0, c0 > 0" "vsmin513")) || // ln 513
        ;; PREDICATE (list "min(t0 * c0, t1 * c0), max(t0, t1) * c0, c0 < 0" "vsmin515")) || // ln 515
(list "n0 && t0" "t0 && n0" "vsand564")
(list "t0 && t0" "t0" "vsand582")
(list "(t0 && n0) && t1" "(t0 && t1) && n0" "vsand585")
        ;; BREAKS REDUCTION ORDER (rewrite(t0 && (t1 && n0), (t0 && t1) && n0, "vsand588")) || // ln 588
(list "n0 || t0" "t0 || n0" "vsor564")
(list "t0 || t0" "t0" "vsor582")
(list "(t0 || n0) || t1" "(t0 || t1) || n0" "vsor585")
(list "t0 || (t1 || n0)" "(t0 || t1) || n0" "vsor588")
(list "n0 == t0" "t0 == n0" "vseq636")
(list "(t0 + n1) == n0" "t0 == (n0 - n1)" "vseq657")
(list "(t0 - n1) == n0" "t0 == (n1 + n0)" "vseq660")
(list "(t0 * -1) == n0" "t0 == (0 - n0)" "vseq670")
        ;; PREDICATE (list "(t0 * c0) == n0" "(t0 * (-1 * c0)) == (n0 * -1), c0 < 0" "vseq675")) || // ln 675
        ;; PREDICATE (list "(t0 * c0) == n0" "(t0 == (n0 / c0)) && ((n0 % c0) == 0)" c0 != 0, "vseq685")) || // ln 685
        ;; BREAKS REDUCTION ORDER (rewrite(t0 == t1, (t0 - t1) == 0, "vseq737")) || // ln 737 // stdlib
(list "n0 != t0" "t0 != n0" "vsne636")
(list "(t0 + n1) != n0" "t0 != (n0 - n1)" "vsne657")
(list "(t0 - n1) != n0" "t0 != (n1 + n0)" "vsne660")
(list "(t0 * -1) != n0" "t0 != (0 - n0)" "vsne670")
        ;; PREDICATE (list "(t0 * c0) != n0" "(t0 * (-1 * c0)) != (n0 * -1)" c0 < 0, "vsne675")) || // ln 675
        ;; PREDICATE (list "(t0 * c0) != n0" "(t0 != (n0/c0)) || ((n0 % c0) != 0)" c0 != 0, "vsne688")) || // ln 688
        ;; BREAKS REDUCTION ORDER (rewrite(t0 != t1, (t0 - t1) != 0, "vsne737")) || // ln 737
(list "n0 < t0" "t0 > n0" "vslt636")
(list "(t0 + n1) < n0" "t0 < (n0 - n1)" "vslt657")
(list "(t0 - n1) < n0" "t0 < (n1 + n0)" "vslt660")
(list "(t0 * -1) < n0" "t0 > (0 - n0)" "vslt670")
        ;; PREDICATE (list "(t0 * c0) < n0, (t0 * (-1 * c0)) > (n0 * -1), c0 < 0" "vslt675")) || // ln 675
        ;; PREDICATE (list "(t0 * c0) < n0, t0 <= ((n0 - 1) / c0), c0 > 0" "vslt693")) || // ln 693
        ;; PREDICATE (list "(t0 / c0) < n0, (t0 / (-1 * c0) * -1) < n0, c0 < 0" "vslt716")) || // ln 716
        ;; PREDICATE (list "(t0 / c0) < n0, t0 < (n0 * c0), c0 > 0" "vslt720")) || // ln 720
        ;; BREAKS REDUCTION ORDER (rewrite(t0 < t1, (t0 - t1) < 0, "vslt737")) || // ln 737 // stdlib
(list "n0 <= t0" "t0 >= n0" "vsle636")
(list "(t0 + n1) <= n0" "t0 <= (n0 - n1)" "vsle657")
(list "(t0 - n1) <= n0" "t0 <= (n1 + n0)" "vsle660")
(list "(t0 * -1) <= n0" "t0 >= (0 - n0)" "vsle670")
        ;; PREDICATE (list "(t0 * c0) <= n0" "(t0 * (-1 * c0)) >= (n0 * -1), c0 < 0" "vsle675")) || // ln 675
        ;; PREDICATE (list "(t0 * c0) <= n0" "t0 <= (n0 / c0), c0 > 0" "vsle691")) || // ln 691
        ;; PREDICATE (list "(t0 / c0) <= n0" "(t0 / (-1 * c0) * -1) <= n0, c0 < 0" "vsle716")) || // ln 716
        ;; PREDICATE (list "(t0 / c0) <= n0, t0 < ((n0 + 1) * c0), c0 > 0" "vsle723")) || // ln 723
        ;; BREAKS REDUCTION ORDER (rewrite(t0 <= t1, (t0 - t1) <= 0, "vsle737")) || // ln 737 // stdlib
(list "n0 > t0" "t0 < n0" "vsgt636")
(list "(t0 + n1) > n0" "t0 > (n0 - n1)" "vsgt657")
(list "(t0 - n1) > n0" "t0 > (n1 + n0)" "vsgt660")
(list "(t0 * -1) > n0" "t0 < (0 - n0)" "vsgt670")
        ;; PREDICATE (list "(t0 * c0) > n0" "(t0 * (-1 * c0)) < (n0 * -1), c0 < 0" "vsgt675")) || // ln 675
        ;; PREDICATE (list "(t0 * c0) > n0" "t0 > (n0 / c0), c0 > 0" "vsgt695")) || // ln 695
        ;; PREDICATE (list "(t0 / c0) > n0" "(t0 / (-1 * c0) * -1) > n0, c0 < 0" "vsgt716")) || // ln 716
        ;; PREDICATE (list "(t0 / c0) > n0" "t0 >= ((n0 + 1) * c0), c0 > 0" "vsgt726")) || // ln 726
        ;; BREAKS REDUCTION ORDER (rewrite(t0 > t1, (t0 - t1) > 0, "vsgt737")) || // ln 737
(list "t0 >= t0" "true" "missingrule2")
(list "n0 >= t0" "t0 <= n0" "vsge636")
(list "(t0 + n1) >= n0" "t0 >= (n0 - n1)" "vsge657")
(list "(t0 - n1) >= n0" "t0 >= (n1 + n0)" "vsge660")
(list "(t0 * -1) >= n0" "t0 <= (0 - n0)" "vsge670")
        ;; PREDICATE (list "(t0 * c0) >= n0" "(t0 * (-1 * c0)) <= (n0 * -1), c0 < 0" "vsge675")) || // ln 675
        ;; PREDICATE (list "(t0 * c0) >= n0" "t0 > ((n0 - 1) / c0), c0 > 0" "vsge697")) || // ln 697
        ;; PREDICATE (list "(t0 / c0) >= n0" "(t0 / (-1 * c0) * -1) >= n0, c0 < 0" "vsge716")) || // ln 716
        ;; PREDICATE (list "(t0 / c0) >= n0" "t0 >= (n0 * c0), c0 > 0" "vsge729")) || // ln 729
        ;; BREAKS REDUCTION ORDER (rewrite(t0 >= t1, (t0 - t1) >= 0, "vsge737")) || // ln 737
))

(define variablesolverTRS
  (map (λ (l) (rule (halide->termIR (first l))
                    (halide->termIR (second l))
                    (third l))) variablesolverTRS-halide))