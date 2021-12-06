#lang racket/base

(require racket/file
         racket/string
         sugar/cache)

(module+ test (require "aoc.rkt"))

(define (get-input str) (map string->number (string-split str ",")))

(define input (get-input (string-trim (file->string "day06.txt"))))

(define/caching (fish-spawns fish-tval days)
  (define first-spawn-day (+ 1 fish-tval))
  (cond
    [(> first-spawn-day days) 0]
    [else
     (define 1st-gen-spawns (+ 1 (quotient (- days first-spawn-day) 7)))
     (+ 1st-gen-spawns
        (for/sum ([days-remaining (in-range (max (- days first-spawn-day) 0) -1 -7)])
          (fish-spawns 8 days-remaining)))]))

(define (fish-after-days lst days)
  (define spawns (map (lambda (n) (fish-spawns n days)) lst))
  (apply + (length lst) spawns))

(define (part-1) (fish-after-days input 80))
(define (part-2) (fish-after-days input 256))

(module+ test
  (check-answer part-1 374994)         ; part-1: 374994 (cpu: 0 real: 0 gc: 0)
  (check-answer part-2 1686252324092)) ; part-2: 1686252324092 (cpu: 0 real: 0 gc: 0)

(module+ test
  (define test-fish '(3 4 3 1 2))
  (check-equal? (fish-after-days test-fish 18) 26)
  (check-equal? (fish-after-days test-fish 80) 5934)
  (check-equal? (fish-after-days test-fish 256) 26984457539))