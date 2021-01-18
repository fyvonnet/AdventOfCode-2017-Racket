#lang racket

(require racket/string)
(require "knot-hash.rkt")

(let ([input (call-with-input-file "inputs/day10" read-line)])
  (displayln (apply * (take (knot-rounds  1 (map string->number (string-split input ","))) 2)))
  (displayln (knot-hash input)))

