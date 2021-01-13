#lang racket


(define (read-input in)
  (let
    ([predicates
       (make-hash
         (list
           (cons ">" >)
           (cons "<" <)
           (cons ">=" >=)
           (cons "<=" <=)
           (cons "!=" (lambda (a b) (not (= a b))))
           (cons "==" =)))]
     [instrs (make-hash (list (cons "inc" +) (cons "dec" -)))])
    (let loop ([in in])
      (match (read-line in)
        [(? eof-object?) null]
        [line
          (cons
            (match (regexp-match #px"^(\\w+) (\\w{3}) ([\\d-]+) if (\\w+) ([<>=!]+) ([-\\d]+)$" line)
              [(list _ reg-instr instr val-instr reg-comp comp val-comp)
               (list
                 (lambda (x) ((hash-ref predicates comp) x (string->number val-comp)))
                 reg-comp
                 (lambda (x) ((hash-ref instrs instr) x (string->number val-instr)))
                 reg-instr
                 )])
            (read-input in))]))))

(define (run-program program)
  (let loop ([lst program] [regs (make-immutable-hash)] [max-value 0])
    (if (null? lst)
      (list (apply max (hash-values regs)) max-value)
      (let*
        ([inst (first lst)])
        (if ((first inst) (hash-ref regs (second inst) 0))
          (let ([new-value ((third inst) (hash-ref regs (fourth inst) 0))])
            (loop
              (rest lst)
              (hash-set regs (fourth inst) new-value)
              (if (> new-value max-value) new-value max-value)))
          (loop (rest lst) regs max-value))))))

(let*
  ([input (call-with-input-file "inputs/day08" read-input)])
  (for ([i (run-program input)]) (println i)))
