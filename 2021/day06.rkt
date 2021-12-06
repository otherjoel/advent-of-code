#lang racket/base

(require racket/file
         racket/string)

(module+ test (require "aoc.rkt"))

(define (get-input str) (map string->number (string-split str ",")))

(define input (get-input (string-trim (file->string "day06.txt"))))

(define (fish-spawns-base fish-tval days)
  (define first-spawn-day (+ 1 fish-tval))
  (cond
    [(> first-spawn-day days) 0]
    [else
     (define 1st-gen-spawns (+ 1 (quotient (- days first-spawn-day) 7)))
     (+ 1st-gen-spawns
        (for/sum ([days-remaining (in-range (max (- days first-spawn-day) 0) -1 -7)])
          (fish-spawns 8 days-remaining)))]))

(define fish-spawns
  (let ([cache (make-hash)])
    (lambda (f d) (hash-ref! cache (cons f d) (λ () (fish-spawns-base f d))))))

(define (fish-after-days lst days)
  (define spawns (map (lambda (n) (fish-spawns n days)) lst))
  (apply + (length lst) spawns))

(define (part-1) (fish-after-days input 80))
(define (part-2) (fish-after-days input 256))

(module+ test
  (check-answer/ns part-1 374994)         ; part-1: 374994 (65 μs)
  (check-answer/ns part-2 1686252324092)) ; part-2: 1686252324092 (213 μs)

(module+ test
  (define test-fish '(3 4 3 1 2))
  (check-equal? (fish-after-days test-fish 18) 26)
  (check-equal? (fish-after-days test-fish 80) 5934)
  (check-equal? (fish-after-days test-fish 256) 26984457539))