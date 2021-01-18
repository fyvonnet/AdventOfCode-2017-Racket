#lang racket

(provide knot-rounds)
(provide knot-hash)

(require racket/string)

(define vec-len 256)

(define (circ-index i)
  (remainder i vec-len))

(define (reverse-section! circ-list len position)
  (let loop ([i position] [j (+ position len -1)])
    (if (<= j i)
      circ-list
      (let ([tmp (vector-ref circ-list (circ-index i))])
        (vector-set! circ-list (circ-index i) (vector-ref circ-list (circ-index j)))
        (vector-set! circ-list (circ-index j) tmp)
        (loop (add1 i) (sub1 j))))))

(define (knot lengths [circ-list (build-vector vec-len identity)] [position 0] [skip-size 0])
  (let loop ([lengths lengths] [position position] [skip-size skip-size])
    (if (null? lengths)
      (values circ-list position skip-size)
      (begin
        (reverse-section! circ-list (car lengths) position)
        (loop
          (cdr lengths)
          (circ-index (+ position skip-size (car lengths)))
          (add1 skip-size))))))

(define (knot-rounds rounds lengths)
  (let loop ([rounds rounds] [circ-lst (build-vector vec-len identity)] [position 0] [skip-size 0])
    (if (zero? rounds)
      (vector->list circ-lst)
      (let-values
        ([(new-circ-lst new-position new-skip-size)
          (knot lengths circ-lst position skip-size)])
        (loop (sub1 rounds) new-circ-lst new-position new-skip-size)))))

(define (int->hex i)
  (let ([symbols "0123456789abcdef"])
    (let-values ([(q r) (quotient/remainder i 16)])
      (list->string
        (for/list ([i (list q r)]) (string-ref symbols i))))))

(define (knot-hash str)
  (let*
    ([lengths
       (append
         (map char->integer (string->list str))
         '(17 31 73 47 23))]
     [sparse-hash (knot-rounds 64 lengths)]
     [xors (map (lambda (block) (int->hex (for/fold ([i 0]) ([b block]) (bitwise-xor i b)))) (slice sparse-hash))])
    (apply string-append xors)))

(define (slice lst)
  (if (null? lst)
    null
    (let-values ([(block rest) (split-at lst 16)])
      (cons block (slice rest)))))

