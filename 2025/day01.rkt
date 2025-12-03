#lang racket/base

(require "../aoc.rkt"
         racket/file
         racket/match
         racket/string)

;(reporting? #t)

(define (parse-line line)
  (match line
    [(regexp #rx"([RL])([0-9]+)" (list _ dir (app string->number num)))
     (* (if (equal? dir "L") -1 1) num)]))

(define input (map parse-line (file->lines "day01-input.txt")))

(define (rotate/zero-clicks pos rotation)
  (define i (+ pos rotation))
  (define j (remainder i 100))
  (values (+ j (if (< j 0) 100 0))
          (+ (abs (quotient i 100))
             (if (and (> pos 0) (< i 1)) 1 0))))

(define (rotations+count-zeros rotations count-proc)
  (for/fold ([pos 50]
             [zero-count 0]
             #:result zero-count)
            ([r (in-list rotations)])
    (define-values (np zs) (rotate/zero-clicks pos r))
    (values np (+ zero-count (count-proc np zs)))))

(define (pt1-count np _zs) (if (= 0 np) 1 0))
(define (day01-1) (rotations+count-zeros input pt1-count))

(check-answer day01-1 1191)

(define (pt2-count _np zs) zs)
(define (day01-2) (rotations+count-zeros input pt2-count))

(check-answer day01-2 6858)

(define example-input
  (map parse-line (string-split
  #<<END
L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
END
  )))

(define (example-pt1)
  (rotations+count-zeros example-input pt1-count))

(define (example-pt2)
  (rotations+count-zeros example-input pt2-count))

(check-answer example-pt1 3)
(check-answer example-pt2 6)