#lang racket/base

(require racket/file
         "../aoc.rkt")

(define (parse-line str)
  (for/list ([c (in-string str)])
    (- (char->integer c) 48)))

(define input (map parse-line (file->lines "day03-input.txt")))

(define (try-joltage bank n digits-remaining)
  (define rest (memq n bank))
  (and rest
       (cond
         [(= 1 digits-remaining) n]
         [else (define next-joltage
                 (for/or ([j (in-range 9 0 -1)])
                   (try-joltage (cdr rest) j (sub1 digits-remaining))))
               (and next-joltage
                    (+ (* (expt 10 (sub1 digits-remaining)) n)
                       next-joltage))])))

(define (max-joltage bank digits)
  (for/or ([i (in-range 9 0 -1)])
    (try-joltage bank i digits)))

(define (total-max-joltage banks digits)
  (for/sum ([b (in-list banks)])
    (max-joltage b digits)))

(define (pt1)
  (total-max-joltage input 2))

(check-answer/ns pt1 17031)

(define (pt2)
 (total-max-joltage input 12))

(check-answer/ns pt2 168575096286051)
