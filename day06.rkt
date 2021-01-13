#lang racket

(require racket/match)
(require racket/string)


(define (find-max banks)
  (let loop ([i 0] [max-val 0] [max-i 0])
    (if (= i (vector-length banks))
      max-i
      (let ([val (vector-ref banks i)])
        (if (> val max-val)
          (loop (add1 i)     val     i)
          (loop (add1 i) max-val max-i))))))

(define (redistribute! banks)
  (let*
    ([max-i (find-max banks)]
     [len (vector-length banks)]
     [nblocks (vector-ref banks max-i)])
    (vector-set! banks max-i 0)
    (let loop ([i (remainder (add1 max-i) len)] [n nblocks])
      (unless (zero? n)
        (vector-set! banks i (add1 (vector-ref banks i)))
        (loop (remainder (add1 i) len) (sub1 n))))))

(define (loop-until-prev-seen banks)
  (let ([seen (make-hash)])
    (let loop ([n 0])
      (match (hash-ref seen banks null)
        [(? null?)
         (begin
           (hash-set! seen (vector-copy banks) n)
           (redistribute! banks)
           (loop (add1 n)))]
        [x (list n (- n x))]))))

(let*
  ([input
     (call-with-input-file "inputs/day06" (lambda (in) (map string->number (string-split (read-line in)))))])
  (for ([i (loop-until-prev-seen (list->vector input))]) (println i)))

