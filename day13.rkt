#lang racket

(struct layer (range depth position moving-down) #:transparent)

(define (decode-line in)
  (match (read-line in)
    [(? eof-object?) null]
    [line
      (match (regexp-match #px"^(\\d+): (\\d+)$" line)
        [(list _ depth range) 
         (layer (string->number range) (string->number depth) 0 #t)])]))

(define (read-input in)
  (let loop ([layer (decode-line in)] [n 0])
    (cond
      [(null? layer) null]
      [(= n (layer-depth layer)) (cons layer (loop (decode-line in) (add1 n)))]
      [else (cons null (loop layer (add1 n)))])))

(define (update-scanners scanners)
  (map
    (lambda (s)
      (match s
        [(? null?) null]
        [(struct layer (range depth position moving-down))
         (if moving-down
           (if (= position (sub1 range))
             (layer range depth (sub1 position) #f)
             (layer range depth (add1 position) #t))
           (if (zero? position)
             (layer range depth (add1 position) #t)
             (layer range depth (sub1 position) #f)))]))
    scanners))

(define (make-initial-range input)
  (if (null? input)
    null
    (cons
      (car input)
      (make-initial-range (update-scanners (cdr input))))))

(define (undetected? range)
  (cond
    [(null? range) #t]
    [(and (not (null? (car range))) (zero? (layer-position (car range)))) #f]
    [else (undetected? (cdr range))]))

(define (find-delay range)
  (let loop ([range range] [delay 0])
    (if (undetected? range)
      delay
      (loop (update-scanners range) (add1 delay)))))

(let*
  ([initial-range
     (make-initial-range
       (call-with-input-file "inputs/day13" read-input))])
  (displayln
    (for/sum ([l initial-range])
      (if (and (not (null? l)) (zero? (layer-position l))) (* (layer-depth l) (layer-range l)) 0)))
  (displayln (find-delay initial-range)))
