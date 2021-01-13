#lang racket

(require racket/string)

(define (distance coord)
  (/ (for/sum ([c coord]) (abs c)) 2))

(let* 
  ([mvmt-hash
     (make-immutable-hash
       (list
         '("n"   0 -1  1)
         '("s"   0  1 -1)
         '("se"  1  0 -1)
         '("nw" -1  0  1)
         '("sw" -1  1  0)
         '("ne"  1 -1  0)))]
   [input
     (map 
       (λ (dir) (hash-ref mvmt-hash dir))
       (call-with-input-file "inputs/day11" (λ (in) (string-split (read-line in) ","))))])
  (for/fold
    ([coord (car input)] [max-dist 0]
     #:result (begin (displayln (distance coord)) (displayln max-dist)))
    ([mvmt  (cdr input)])
    (let* ([new-coord (map + coord mvmt)] [dist (distance new-coord)])
      (values new-coord (if (> dist max-dist) dist max-dist)))))

