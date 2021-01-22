#lang racket

(require math/array)
(require "knot-hash.rkt")

(define grid-side 128)
(define grid-area (expt grid-side 2))

(define (int->bin i)
  (let loop ([i i] [x 4] [lst '()])
    (if (zero? x)
      lst
      (let-values ([(q r) (quotient/remainder i 2)])
        (loop q (sub1 x) (cons r lst))))))

(define (make-grid input hex-hash)
  (for/list
    ([i (in-range grid-side)])
    (for/fold
      ([lst '()])
      ([sym (string->list (knot-hash (format "~a-~a" input i)))])
      (append lst (hash-ref hex-hash sym)))))

(define (explore-region grid start-x start-y)
  (let loop ([st grid] [stack (list (cons start-x start-y))])
    (if (null? stack)
      st
      (let ([coord (car stack)])
        (if (set-member? st coord)
          (loop
            (set-remove st coord)
            (for/fold
              ([new-stack (cdr stack)])
              ([x-shift '(1  0 -1  0)]
               [y-shift '(0  1  0 -1)])
              (let
                ([neighb-coord
                   (cons
                     (+ (car coord)  x-shift)
                     (+ (cdr coord)  y-shift))])
                (if (set-member? st neighb-coord)
                  (cons neighb-coord new-stack)
                  new-stack))))
          (loop st (cdr stack)))))))

(define (count-regions grid)
  (let loop ([st grid] [n 0] [count 0])
    (if (= n grid-area)
      count
      (let-values ([(x y) (quotient/remainder n grid-side)])
        (if (set-member? st (cons x y))
          (loop (explore-region st x y) (add1 n) (add1 count))
          (loop st (add1 n) count))))))

(let*
  ([input (call-with-input-file "inputs/day14" read-line)]
   [hex-hash
     (for/hash
       ([sym (string->list "0123456789abcdef")]
        [i (in-naturals)])
       (values sym (int->bin i)))]
   [grid-set
     (for/fold
       ([s (set)])
       ([y (in-naturals)]
        [grid-line (make-grid input hex-hash)])
       (set-union
         s
         (for/set
           ([x (in-naturals)]
            [grid-square grid-line]
            #:when (= 1 grid-square))
           (cons x y))))])
  (displayln (set-count grid-set))
  (displayln (count-regions grid-set)))
