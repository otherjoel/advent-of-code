#lang racket/base

(require "aoc.rkt"
         racket/file
         racket/math)

(define input (map (lambda (s) (string->number s 2)) (file->lines "day03.txt")))

(define (bit-at num pos) (sgn (bitwise-and num (expt 2 (- pos 1)))))

(define-values (most least) (values >= <))

(define (common-bitval comp lst pos)
  (if (comp (apply + (map (lambda (n) (bit-at n pos)) lst))
            (/ (length lst) 2))
      1 0))

(define (get-rate lst comp)
  (for/sum ([i (in-range 12 0 -1)])
    (* (common-bitval comp input i) (expt 2 (- i 1)))))

(define (part-1)
  (* (get-rate input most)
     (get-rate input least)))

(module+ test
  (check-answer part-1 1307354)) ; part-1: 1307354 (cpu: 1 real: 1 gc: 0)

(define (filter-numbers comp)
  (let loop ([lst input]
             [pos 12])
    (define filter-bit (common-bitval comp lst pos))
    (define remaining (filter (lambda (n) (eq? (bit-at n pos) filter-bit)) lst))
    (if (null? (cdr remaining))
        (car remaining)
        (loop remaining (- pos 1)))))

(define (part-2)
  (* (filter-numbers most)
     (filter-numbers least)))

(module+ test
  (check-answer part-2 482500)) ; part-2: 482500 (cpu: 0 real: 0 gc: 0)