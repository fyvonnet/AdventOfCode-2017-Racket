#lang racket

(require racket/string)


(define (read-input in)
  (match (read-line in)
    [(? eof-object?) null]
    [line
      (cons
        (match (regexp-match #px"^(\\w+) \\((\\d+)\\)( -> (.*))?$" line)
          [(list _ name weight _ children)
           (list
             name
             (string->number weight)
             (if children (string-split children ", ") null))])
        (read-input in))]))

(define (build-tree progs-hash root)
  (match (hash-ref progs-hash root)
    [(cons weight children)
     (let
       ([children-trees (map (lambda (c) (build-tree progs-hash c)) children)])
       (list
         (+ weight (for/sum ([t children-trees]) (car t)))
         weight
         children-trees))]))

(define (correct-imbalance root)
  (let*
    ([children (third root)]
     [min-val (apply min (map first children))]
     [max-val (apply max (map first children))])
    (if (= min-val max-val)
      null
      (let*-values
        ([(a b) (partition (lambda (c) (= min-val (first c))) children)]
         [(unbalanced correction)
          (if (= 1 (length a))
            (values (first a) (- max-val min-val))
            (values (first b) (- min-val max-val)))])
        (match (correct-imbalance unbalanced)
          ((? null?) (+ correction (second unbalanced)))
          (x x))))))

(let*
  ([input (call-with-input-file "inputs/day07" read-input)]
   [progs-hash
     (for/hash
       ([prog input])
       (match prog
         [(list name weight children) (values name (cons weight children))]))]
   [root
     (for/fold
       ([names-set (list->set (map first input))]
        #:result (first (set->list names-set)))
       ([prog input])
       (set-subtract names-set (list->set (third prog))))]
   [values-tree (build-tree progs-hash root)])
  (displayln root)
  (displayln (correct-imbalance values-tree)))
