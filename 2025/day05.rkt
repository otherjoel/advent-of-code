#lang racket/base

(require "../aoc.rkt"
         racket/file
         (only-in racket/list splitf-at)
         data/integer-set
         racket/string
         threading)

;; Part 1 ----------------------------------------

(define (string->set str)
  (~> (string-split str "-")
      (map string->number _)
      (apply cons _)
      list
      make-integer-set))

(define (parse lines)
  (define-values (range-lines ingred-lines) (splitf-at lines non-empty-string?))
  (define fresh-set
    (for/fold ([s (make-integer-set '())])
              ([r (in-list range-lines)])
      (union s (string->set r))))
  (define ingreds
    (for/fold ([s (make-integer-set '())])
              ([i (in-list (cdr ingred-lines))])
      (define id (string->number i))
      (union s (make-integer-set `((,id . ,id))))))
  (values fresh-set ingreds))

(define-values (f i) (parse (file->lines "day05-input.txt")))

(define (pt1) (count (intersect f i)))
(check-answer/ns pt1 563)

;; Part 2 ----------------------------------------

(define (pt2) (count f))
(check-answer/ns pt2 338693411431456)