#lang racket/base

(require racket/file
         threading)

(define (fuel-required mass)
  (- (quotient mass 3) 2))

(define (recurse-fuel-required mass)
  (define f (fuel-required mass))
  (cond
    [(not (positive? f)) 0]
    [else (+ f (recurse-fuel-required f))]))

(define (day01-part1)
  (~>> (file->lines "day01-input.txt")
       (map string->number)
       (map fuel-required)
       (apply +)))

(define (day01-part2)
  (~>> (file->lines "day01-input.txt")
       (map string->number)
       (map recurse-fuel-required)
       (apply +)))