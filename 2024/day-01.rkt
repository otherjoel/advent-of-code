#lang racket/base

(require "../aoc.rkt"
         racket/file
         racket/string)

(define example-left-list (list 3 4 2 1 3 3))
(define example-right-list (list 4 3 5 3 9 3))

(define (find-total-distance left right)
  (for/sum ([left-value (in-list (sort left <))]
            [right-value (in-list (sort right <))])
    (abs (- left-value right-value))))

(define (similarity-score left right)
  (define count-occurences
    (let ([memo (make-hasheqv)])
      (lambda (n)
        (hash-ref! memo n (λ () (length (filter (λ (v) (= v n)) right)))))))
  (for/sum ([left-val (in-list left)])
    (* left-val (count-occurences left-val))))

(find-total-distance example-left-list example-right-list)

(define (load-input filename)
  (define input-lines (file->lines filename))
  (define number-lines
    (for/list ([line (in-list input-lines)])
      (map string->number (string-split line))))
  (define left-list (map car number-lines))
  (define right-list (map cadr number-lines))
  (list left-list right-list))

(define input-lists (load-input "day-01-input.txt"))
(define left (car input-lists))
(define right (cadr input-lists))

(define (day01-part1)
  (find-total-distance left right))

(check-answer/ns day01-part1 2086478)

(similarity-score example-left-list example-right-list)

(define (day01-part2)
  (similarity-score left right))

(check-answer/ns day01-part2 24941624)
