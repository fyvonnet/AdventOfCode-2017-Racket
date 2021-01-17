#lang racket

(struct layer (range depth position moving-down) #:transparent)

(define (read-input in)
  (match (read-line in)
    [(? eof-object?) '()]
    [line 
      (cons
        (match (regexp-match #px"^(\\d+): (\\d+)$" line)
          [(list _ depth range) 
           (layer (string->number range) (string->number depth) 0 #t)])
        (read-input in))]))

(define (update-scanners scanners)
  (if (null? scanners)
    null
    (cons
      (match (car scanners)
        [(struct layer (range depth position moving-down))
         (if moving-down
           (if (= position (sub1 range))
             (layer range depth (sub1 position) #f)
             (layer range depth (add1 position) #t))
           (if (zero? position)
             (layer range depth (add1 position) #t)
             (layer range depth (sub1 position) #f)))])
      (update-scanners (cdr scanners)))))

(define (move scanners)
  (let loop ([lst scanners] [l 0] [severity 0])
    (match lst
      [(? null?) severity]
      [(cons (struct layer (range depth position _)) rest-scanners)
       (if (= l depth)
         (loop
           (update-scanners rest-scanners)
           (add1 l)
           (if (zero? position)
             (+ severity (* range depth))
             severity))
         (loop (update-scanners lst) (add1 l) severity))])))

(let*
  ([input (call-with-input-file "inputs/day13" read-input)])
  (displayln (move input)))
