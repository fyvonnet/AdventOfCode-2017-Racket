#lang racket


(define (read-input in)
  (match (read-line in)
    [(? eof-object?) null]
    [line
      (cons
        (string->number line)
        (read-input in))]))

(define (trampoline input func)
  (let
    ([len (length input)]
     [vec (list->vector input)])
    (let loop ([i 0] [count 0])
      (if (or (< i 0) (>= i len))
        count
        (let ([off (vector-ref vec i)])
          (vector-set! vec i (func off))
          (loop (+ i off) (add1 count)))))))

(let*
  ([input (call-with-input-file "inputs/day05" read-input)])
  (println (trampoline input add1))
  (println (trampoline input (lambda (off) (if (>= off 3) (sub1 off) (add1 off))))))
