#lang racket

(define factors (list 16807 48271))
(define divisor 2147483647)
(define mask (sub1 (expt 2 16)))

(define (read-input in)
  (match (read-line in)
    [(? eof-object?) '()]
    [line
      (cons
        (string->number
          (second
            (regexp-match #px"^Generator . starts with (\\d+)$" line)))
        (read-input in))]))

(define (generator vals)
  (let loop ([vs vals] [count 40000000] [cnt 0])
    (if (zero? count)
      cnt
      (loop
        (map
          (lambda (n) (remainder n divisor))
          (map * vs factors))
        (sub1 count)
        (match (map (lambda (n) (bitwise-and n mask)) vs)
          [(list a b)
           (if (= a b) (add1 cnt) cnt)])))))

(let
  ([input (call-with-input-file "inputs/day15" read-input)])
  (displayln (generator input)))
