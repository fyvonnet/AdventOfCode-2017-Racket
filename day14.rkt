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

(define (valid? i)
  (and (>= i 0) (< i grid-side)))

(define (explore-region! grid start-x start-y)
  (let loop ([stack (list (cons start-x start-y))])
    (unless (null? stack)
      (match (car stack)
        [(cons x y)
         (if (zero? (array-ref grid (vector x y)))
           (loop (cdr stack))
           (begin
             (array-set! grid (vector x y) 0)
             (loop
               (for/fold
                 ([new-stack (cdr stack)])
                 ([x-shift '(1  0 -1  0)]
                  [y-shift '(0  1  0 -1)])
                 (let ([neighb-x (+ x x-shift)] [neighb-y (+ y y-shift)] )
                   (if
                     (and
                       (valid? neighb-x)
                       (valid? neighb-y)
                       (= 1 (array-ref grid (vector neighb-x neighb-y))))
                     (cons (cons neighb-x neighb-y) new-stack)
                     new-stack))))))]))))

(define (count-regions grid)
  (let loop ([n 0] [count 0])
    (if (= n grid-area)
      count
      (let-values ([(x y) (quotient/remainder n grid-side)])
        (if (= 1 (array-ref grid (vector x y)))
          (begin
            (explore-region! grid x y)
            (loop (add1 n) (add1 count)))
          (loop (add1 n) count))))))

(let*
  ([input "ljoxqyyw"]
   [hex-hash
     (for/hash
       ([sym (string->list "0123456789abcdef")]
        [i (in-range 16)])
       (values sym (int->bin i)))]
   [grid-lst (make-grid input hex-hash)]
   [grid (list*->array grid-lst number?)])
  (displayln (for/sum ([line grid-lst]) (for/sum ([i line]) i)))
  (displayln (count-regions grid)))
