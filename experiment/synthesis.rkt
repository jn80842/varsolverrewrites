#lang rosette

(require "../traat/termIR.rkt")
(require "../halide-parser.rkt")
(require "../saturation-synthesis.rkt")

;;; read in inputs.txt

;;; read in previous TRS and blacklist

;;; do synthesis

;;; write out new TRS and blacklist

;;; logging from synthesis goes in log.txt

(with-input-from-file "inputs.txt"
  (λ ()
    (define inputs
      (for/list ([line (in-lines)])
        (halide->termIR line)))
    (with-input-from-file "TRS.txt"
      (λ ()
        (define TRS
          (for/list ([line (in-lines)])
            (string->rule line)))
        (with-input-from-file "blacklist.txt"
          (λ ()
            (define blacklist
              (for/list ([line (in-lines)])
                (halide->termIR line)))
            (define synth-output (recursive-synthesis inputs TRS blacklist))
            (display-lines-to-file (map rule->string (first synth-output)) "new-TRS.txt" #:mode 'text #:exists 'append)
            (display-lines-to-file (map termIR->halide (second synth-output)) "new-blacklist.txt" #:mode 'text #:exists 'append)))))))