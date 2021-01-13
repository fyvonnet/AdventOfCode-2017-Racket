#lang racket

(require racket/string)


(define (read-input in)
  (match (read-line in)
    [(? eof-object?) null]
    [line (cons (string-split line) (read-input in))]))

(define (valid-passphrase? line)
  (let loop ([lst (sort line string<?)])
    (cond
      [(null? (cdr lst)) #t]
      [(string=? (car lst) (cadr lst)) #f]
      [else (loop (cdr lst))])))

(define (count-valid-passphrases input)
  (for/sum
    ([line input])
    (if (valid-passphrase? line) 1 0)))

(let*
  ([input (call-with-input-file "inputs/day04" read-input)])
  (println (count-valid-passphrases input))
  (println
    (count-valid-passphrases
      (map
        (lambda (line)
          (map
            (lambda (word) (list->string (sort (string->list word) char<?)))
            line))
        input))))
