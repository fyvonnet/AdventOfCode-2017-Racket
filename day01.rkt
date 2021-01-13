#lang racket


(define (read-input in)
  (match (read-char in)
    [(? char-numeric? c)
     (cons 
       (- (char->integer c) (char->integer #\0))
       (read-input in))]
    [_ null]))

(define (sum-equals lst-a lst-b)
  (for/sum ([a lst-a] [b lst-b]) (if (= a b) a 0)))

(let* 
  ([input (call-with-input-file "inputs/day01" read-input)]
   [inputinput (append input input)])
  (println (sum-equals input (cdr inputinput)))
  (println (sum-equals input (drop inputinput (/ (length input) 2)))))
