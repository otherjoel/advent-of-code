#lang debug racket/base

(require racket/file
         racket/match
         racket/list
         racket/math
         racket/vector
         threading)

(module+ test (require "aoc.rkt"))

(define (parse-input lines)
  (for/list ([line (in-list lines)])
    (match
        (~>> (regexp-match #rx"([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)" line)
             cdr
             (map string->number))
      [(list x1 y1 x2 y2) (list (list x1 y1) (list x2 y2))])))

(define input (parse-input (file->lines "day05.txt")))

(define (line->pts line)
  (match-define (list (list x1 y1) (list x2 y2)) line)
  (define pt-ct (+ 1 (max (abs (- y2 y1)) (abs (- x2 x1)))))
  (define (step-coord v1 v2)
    (if (eqv? v1 v2) (make-list pt-ct v1) (inclusive-range v1 v2 (sgn (- v2 v1)))))
  (for/vector ([x (in-list (step-coord x1 x2))]
               [y (in-list (step-coord y1 y2))])
    (make-rectangular x y)))
  
(define (horiz/vert? line)
  (match-define (list (list x1 y1) (list x2 y2)) line)
  (or (eqv? x1 x2) (eqv? y1 y2)))

(define (duplicate-pts pts)
  (define seen (make-hasheqv))
  (for ([pt (in-vector pts)])
    (hash-update! seen pt add1 0))
  (for/vector ([(pt ct) (in-hash seen)]
               #:when (> ct 1))
    pt))

(define (count-overlap-pts lines)
  (~>> (map line->pts lines)
       (apply vector-append)
       duplicate-pts
       vector-length))

(define (part-1 [lines input]) (count-overlap-pts (filter horiz/vert? lines)))

(define (part-2 [lines input]) (count-overlap-pts lines))

(module+ test
  (check-answer part-1 5169)   ; part-1: 5169 (cpu: 138 real: 139 gc: 9) 
  (check-answer part-2 22083)) ; part-2: 22083 (cpu: 500 real: 500 gc: 16)ß

(module+ test
  (define test-input
    (parse-input
     '("0,9 -> 5,9"
       "8,0 -> 0,8"
       "9,4 -> 3,4"
       "2,2 -> 2,1"
       "7,0 -> 7,4"
       "6,4 -> 2,0"
       "0,9 -> 2,9"
       "3,4 -> 1,4"
       "0,0 -> 8,8"
       "5,5 -> 8,2")))
  (check-equal? (part-1 test-input) 5)
  (check-equal? (part-2 test-input) 12))