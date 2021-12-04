#lang racket/base

(require "aoc.rkt")

(define line-count (/ (file-size "day03.txt") 13))
(define halfway (/ line-count 2))
(define cols (make-vector 12 0))

(define (read-cols! [rem (- line-count 1)] [col 0] [char (read-byte)])
  (cond
    [(eof-object? char) (void)]
    [(eq? char 10) (read-cols! (- rem 1) 0)] ; 10 = newline
    [else
     (define col-val (vector-ref cols col))
     (cond
       [(char? col-val) (read-cols! rem (+ col 1))]
       [else
        (define sum (+ col-val (- char 48)))
        (define result
          (cond [(> sum halfway) #\1] ; when 1s have already won
                [(< (+ sum rem) halfway) #\0] ; when 0s have already won
                [else sum]))
        (vector-set! cols col result)
        (read-cols! rem (+ col 1))])]))

(define (calc)
  (for/fold ([gamma 0]
             [epsilon 0]
             #:result (* gamma epsilon))
            ([c (in-vector cols)]
             [i (in-range 11 -1 -1)])
    (values (+ gamma (* (if (eq? c #\1) 1 0) (expt 2 i)))
            (+ epsilon (* (if (eq? c #\1) 0 1) (expt 2 i))))))

(define (part-1)
  (with-input-from-file "day03.txt" read-cols!)
  (calc))

(module+ test
  (check-answer part-1 1307354))

(require racket/file
         racket/math)

(define input (map (lambda (s) (string->number s 2)) (file->lines "day03.txt")))

(define (bit-at num pos)
  (sgn (bitwise-and num (expt 2 (- pos 1)))))

(define most >=)
(define least <)
(define (common-bitval comp lst pos)
  (if (comp (apply + (map (lambda (n) (bit-at n pos)) lst))
            (/ (length lst) 2))
      1 0))

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
  (check-answer part-2 482500))

(define (get-rate lst comp)
  (for/sum ([i (in-range 12 0 -1)])
    (* (common-bitval comp input i) (expt 2 (- i 1)))))

(define (part1-redux)
  (* (get-rate input most)
     (get-rate input least)))

(module+ test
  (check-answer part1-redux 1307354))