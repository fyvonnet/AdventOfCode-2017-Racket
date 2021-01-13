#lang racket


(define (read-garbage in)
  (let loop ([count 0])
    (case (read-char in)
      [(#\>) count]
      [(#\!) (begin (read-char in) (loop count))]
      [else  (loop (add1 count))])))

(define (read-stream in)
  (let loop ([group-score 1] [total-score 0] [garbage-count 0])
    (match (read-char in)
      [(? eof-object?) (list total-score garbage-count)]
      [#\{ (loop (add1 group-score) (+ group-score total-score) garbage-count)]
      [#\} (loop (sub1 group-score) total-score garbage-count)]
      [#\< (loop group-score total-score (+ garbage-count (read-garbage in)))]
      [_   (loop group-score total-score garbage-count)])))

(for ([i (call-with-input-file  "inputs/day09" read-stream)]) (println i))
