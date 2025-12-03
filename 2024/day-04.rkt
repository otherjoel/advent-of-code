#lang racket/base

(require "../aoc.rkt"
         racket/file
         racket/match
         racket/string
         threading)

(struct grid (rows cols vec))

(define (grid-ref g x y)
  (vector-ref (grid-vec g) (+ (* y (grid-cols g)) x)))

(define n 0-1i)
(define s 0+1i)
(define e 1+0i)
(define w -1+0i)
(define ne 1-1i)
(define nw -1-1i)
(define se 1+1i)
(define sw -1+1i)

(define (ref-dir g startx starty dir)
  (define-values (newx newy)
    (values (+ startx (real-part dir)) (+ starty (imag-part dir))))
  (if (and (< -1 newx (grid-cols g)) (< -1 newy (grid-rows g)))
      (grid-ref g newx newy)
      #f))

(define (lines->grid lines)
  (define rows (length lines))
  (define cols (string-length (car lines)))
  (define vec
    (for*/vector ([line (in-list lines)]
                  [ch (in-string line)])
      ch))
  (grid rows cols vec))

(define input-grid
  (~> (file->lines "day-04-input.txt")
      lines->grid))

(define (count-xmas g)
  (for*/sum ([x (in-range (grid-cols g))]
             [y (in-range (grid-rows g))])
    (cond
      [(char=? (grid-ref g x y) #\X)
       (for/sum ([dir (in-list (list n ne e se s sw w nw))])
         (cond
           [(eqv? (ref-dir g x y dir) #\M)
            (if (and (eqv? #\A (ref-dir g x y (* dir 2)))
                     (eqv? #\S (ref-dir g x y (* dir 3))))
                1 0)]
           [else 0]))]
      [else 0])))

(define (part1)
  (count-xmas input-grid))

(check-answer/ns part1 2562) ; 1 ms

(define (xshape-at? g x y)
  (cond
    [(char=? (grid-ref g x y) #\A)
     (match (list (list (ref-dir g x y sw) (ref-dir g x y ne))
                  (list (ref-dir g x y se) (ref-dir g x y nw)))
       [(list-no-order (list-no-order #\S #\M) (list-no-order #\S #\M)) 1]
       [_ 0])]
    [else 0]))

(define (count-x-shapes g)
  (for*/sum ([x (in-range (grid-cols g))]
             [y (in-range (grid-rows g))])
    (xshape-at? g x y)))

(define (part2)
  (count-x-shapes input-grid))

(check-answer/ns part2 1902) ; 463 μs