#lang racket

(define (turn-left direction)
  (case direction
    ['north 'west ]
    ['west  'south]
    ['south 'east ]
    ['east  'north]))

(define (move-forward direction coord)
  (let ([x (car coord)] [y (cdr coord)])
    (case direction
      ['north (cons x (sub1 y))]
      ['west  (cons (sub1 x) y)]
      ['south (cons x (add1 y))]
      ['east  (cons (add1 x) y)])))

(define (coord+ a b)
  (cons
    (+ (car a) (car b))
    (+ (cdr a) (cdr b))))

(define (create-spiral steps)
  (let loop ([coords '((0 . 0))] [segment 2] [steps (sub1 steps)] [segment-steps 1] [direction 'east])
    (cond
      [(zero? steps)
       (values
         (+ (abs (caar coords)) (abs (cdar coords)))
         (cdr (reverse coords)))]
      [(zero? segment-steps)
       (loop
         coords
         (add1 segment)
         steps
         (quotient (add1 segment) 2)
         (turn-left direction))]
      [else
        (loop
          (cons (move-forward direction (car coords)) coords)
          segment
          (sub1 steps)
          (sub1 segment-steps)
          direction)])))

(define (solve-part-2 input spiral)
  (let loop ([lst spiral] [ht (make-immutable-hash '(((0 . 0) . 1)))])
    (let
      ([value
         (for/sum
           ([neighb '((-1 . -1) (0 . -1) (1 . -1)
                      (-1 .  0)          (1 .  0)
                      (-1 .  1) (0 .  1) (1 .  1))])
           (hash-ref ht (coord+ neighb (car lst)) 0))])
      (if (> value input)
        value
        (loop (cdr lst) (hash-set ht (car lst) value))))))

(let*
  ([input (call-with-input-file "inputs/day03" (lambda (in) (string->number (read-line in))))])
  (let-values
    ([(distance spiral) (create-spiral input)])
    (println distance)
    (println (solve-part-2 input spiral))))
