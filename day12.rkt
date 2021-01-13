#lang racket

(require data/queue)

(define (read-input in)
  (match (read-line in)
    [(? eof-object?) null]
    [line 
      (cons
        (match (regexp-match #px"^(\\d+) <-> (.+)$" line)
          [(list _ id connexions) (map string->number (cons id (string-split connexions ", ")))])
        (read-input in))]))

(define (explore-group pipes start)
  (let ([queue (make-queue)])
    (enqueue! queue start)
    (let loop ([excluded (set start)])
      (if (queue-empty? queue)
        excluded
        (let* ([prog (dequeue! queue)])
          (loop
            (for/fold
              ([new-excluded excluded])
              ([p (hash-ref pipes prog)])
              (if (set-member? excluded p)
                new-excluded
                (begin
                  (enqueue! queue p)
                  (set-add new-excluded p))))))))))

(define (count-groups pipes-hash progs-set)
  (if (set-empty? progs-set)
    1
    (add1
      (count-groups
        pipes-hash
        (set-subtract progs-set (explore-group pipes-hash (set-first progs-set)))))))

(let*
  ([input (call-with-input-file "inputs/day12" read-input)]
   [pipes-hash (make-immutable-hash input)]
   [progs-set (list->set (hash-keys pipes-hash))]
   [group0-set (explore-group pipes-hash 0)])
  (println (set-count group0-set))
  (println (count-groups pipes-hash (set-subtract progs-set group0-set))))

