#lang racket/base

(require "aoc.rkt"
         racket/file
         racket/match
         racket/vector
         rackunit
         threading)

(define input (~>> (file->lines "day01.txt")
                   (map string->number)
                   list->vector))

(module+ test (define test #(199 200 208 210 200 207 240 269 260 263)))

(define (part-1 [vec input])
  (for/fold ([sum 0]
             [previous #f]
             #:result sum)
            ([measurement (in-vector vec)])
    (cond
      [(and previous (> measurement previous))
       (values (+ sum 1) measurement)]
      [else (values sum measurement)])))

(module+ test
  (check-equal? (part-1 test) 7)
  (check-answer part-1 1482))

(define (part-2 [vec input])
  (let loop ([increased-count 0]
             [prev-sum #f]
             [remaining vec])
    (cond
      [(< (vector-length remaining) 3) increased-count]
      [else
       (define current-window-sum
         (match remaining [(vector m1 m2 m3 _ ...) (+ m1 m2 m3)]))
       (loop (if (and prev-sum (> current-window-sum prev-sum)) (+ increased-count 1) increased-count)
             current-window-sum
             (vector-drop remaining 1))])))
    
(module+ test
  (check-equal? (part-2 test) 5)
  (check-answer part-2 1518))