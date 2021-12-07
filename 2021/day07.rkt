#lang racket/base

(require racket/file
         racket/string
         racket/vector)

(module+ test (require "aoc.rkt"))

(define (parse str)
  (list->vector (map string->number (string-split (string-trim str) ","))))

(define input (parse (file->string "day07.txt")))

(define (optimal-pos vec) ; median
  (vector-ref (vector-sort vec <) (ceiling (/ (vector-length vec) 2))))
  
(define (total-fuel-to vec pos) (for/sum ([v (in-vector vec)]) (abs (- v pos))))

(define (part-1) (total-fuel-to input (optimal-pos input)))

(module+ test (check-answer/ns part-1 342641)) ; part-1: 342641 (68 μs)

(define (average vec) (/ (for/sum ([v (in-vector vec)]) v) (vector-length vec)))

(define (fuel-to/new vec pos)
  (for/sum ([v (in-vector vec)])
    (let ([dist (abs (- v pos))]) (/ (* dist (+ 1 dist)) 2)))) ; n(n+1)/2 = sum of 1 to n

(define (optimal-fuel vec)
  (let ([avg (average vec)])
    (min (fuel-to/new vec (floor avg))      ; checking both is the only way that works
         (fuel-to/new vec (ceiling avg))))) ; on both the test and real inputs!

(define (part-2) (optimal-fuel input))
(module+ test (check-answer/ns part-2 93006301)) ; part-2: 93006301 (116 μs)
(module+ test
  (define test-input (parse "16,1,2,0,4,2,7,1,2,14"))

  (check-equal? (optimal-pos test-input) 2)
  (check-equal? (total-fuel-to test-input 2) 37)
  (check-equal? (total-fuel-to test-input 1) 41)
  (check-equal? (total-fuel-to test-input 3) 39)
  (check-equal? (total-fuel-to test-input 10) 71)

  (check-equal? (fuel-to/new test-input 5) 168)
  (check-equal? (fuel-to/new test-input 2) 206)
  (check-equal? (optimal-fuel test-input) 168))