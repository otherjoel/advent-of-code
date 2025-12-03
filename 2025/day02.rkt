#lang debug racket/base

(require "../aoc.rkt"
         racket/file
         racket/list
         racket/match
         racket/string
         threading)

;(reporting? #t)

(define (parse-data str)
  (for/list ([pr (in-list (string-split str #px",\\s*"))])
    (map string->number (string-split pr "-"))))

(define input
  (~> (file->string "day02-input.txt")
      parse-data))

;; Wrong for n=0 but fine for here
(define (digit-count n)
  (let loop ([n n] [count 0])
    (if (zero? n) count (loop (quotient n 10) (add1 count)))))

(define (repeated-sequence?/pt1 n)
    (let ([num-digits (digit-count n)])
      (or (and (even? num-digits)
               (let ([divisor (+ (expt 10 (quotient num-digits 2)) 1)])
                 (or (and (zero? (remainder n divisor)) n) 0)))
          0)))

(define (proper-divisors d)
  ;; All divisors of d that are less than d
  (for/list ([k (in-range 1 d)]
             #:when (zero? (remainder d k)))
    k))

(define (k-digit-number? s k)
  ;; Is s a number with exactly k digits?
  (if (= k 1)
      (<= 1 s 9)
      (and (>= s (expt 10 (sub1 k)))
           (< s (expt 10 k)))))

(define (repeated-sequence?/pt2 n)
  (let ([d (digit-count n)])
    (for/or ([k (in-list (proper-divisors d))])
      (let* ([repunit (quotient (sub1 (expt 10 d))
                                (sub1 (expt 10 k)))]
             [remainder-val (remainder n repunit)])
        (and (zero? remainder-val)
             (k-digit-number? (quotient n repunit) k)
             n)))))

(define (sum-invalids prs [proc repeated-sequence?/pt1])
  (for*/sum ([pr (in-list prs)]
             [id (apply in-inclusive-range pr)])
    (or (proc id) 0)))

(define (pt1)
  (sum-invalids input))

(check-answer pt1 54234399924)

(define (pt2)
  (sum-invalids input repeated-sequence?/pt2))

(check-answer pt2 70187097315)

;;================================================

(define example-input (parse-data #<<END
11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124
END
                                  ))

(check-answer (λ () (sum-invalids example-input)) 1227775554)