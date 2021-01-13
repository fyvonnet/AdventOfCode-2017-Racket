#lang racket

(require racket/string)


(define (read-input in)
  (match (read-line in)
    [(? eof-object?) null]
    [line
      (cons
        (sort (map string->number (string-split line)) >)
        (read-input in))]))

(define (search-dividing-numbers-sub a lst)
  (cond
    ((null? lst) null)
    ((zero? (remainder a (car lst))) (quotient a (car lst)))
    (else (search-dividing-numbers-sub a (cdr lst)))))

(define (search-dividing-numbers lst)
  (match (search-dividing-numbers-sub (car lst) (cdr lst))
    [(? null?) (search-dividing-numbers (cdr lst))]
    [result result]))

(let*
  ([input (call-with-input-file "inputs/day02" read-input)])
  (println (for/sum ([line input]) (- (first line) (last line))))
  (println (for/sum ([line input]) (search-dividing-numbers line))))
