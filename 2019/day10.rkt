#lang racket

(require rackunit
         threading)

;; Create a list of points that contain asteroids.
;; Points are complex numbers, the real portion = x coordinate,
;; and the imaginary portion is the y coordinate.
(define (string->asteroid-pts str)
  (let* ([rows (map string->list (string-split str))]
         [height (length rows)]
         [width (length (car rows))]
         [all-points (flatten rows)])
    (for/fold ([asters '()])
              ([reading (in-list all-points)]
               [n (in-range (length all-points))])
      (cond [(equal? reading #\#)
             (define y (quotient n width))
             (define x (- n (* y width)))
             (cons (make-rectangular x y) asters)]
            [else asters]))))

;; Returns a hash table of asteroids visible from the origin point.
;; The key is the angle from the origin point, the value is the distance
;; to the origin point.
(define (visible asteroids origin)
  (for/fold ([already-visible (hash)])
            ([asteroid-pt (in-list asteroids)])
    (cond
      [(equal? origin asteroid-pt) already-visible] ; skip start pt
      [else
       (define dist (magnitude (- origin asteroid-pt)))
       (define ang (angle (- origin asteroid-pt)))
       (cond
         ;; See if this asteroid is the closest one seen at this angle
         [(< dist (hash-ref already-visible ang +inf.0))
          (hash-set already-visible ang dist)]
         [else already-visible])])))

;; For the asteroid from which the most asteroids are visible,
;; return '(point . <visible asteroid count>)
(define (best-asteroid asteroids)
  (define rankings
    (for/hash ([here (in-list asteroids)])
      (let ([c (hash-count (visible asteroids here))])
        (values c (cons here c)))))
  (hash-ref rankings (apply max (hash-keys rankings))))

(define input (string->asteroid-pts (file->string "day10-input.txt")))

(define (day10-p1)
  (best-asteroid input))

(check-equal? (time (day10-p1))
              '(20+19i . 284)) ;; Coordinates and answer for part one

;; Converts an '(angle . magnitude) pair back into an absolute coordinate
;; using another point as its relative origin
(define (am->num origin p)
  (define n (make-polar (cdr p) (car p)))
  (- origin (make-rectangular (inexact->exact (round (real-part n)))
                              (inexact->exact (round (imag-part n))))))

;; Like the `visible` function above, but sorts the resulting list of
;; '(angle . magnitude) pairs into clockwise order starting with those
;; at (pi / 2) radians (= north on the grid). Then converts those back
;; into absolute coordinates.
(define (visible-clockwise-from-north asteroids origin)
  (define (not-upper-right-quadrant? p)
    (not (<= (* pi 1/2) (car p) pi)))
  (define-values (end begin)
    (~> (visible asteroids origin)
        hash->list
        (sort < #:key car)
        (splitf-at not-upper-right-quadrant?)))
  (map (curry am->num origin) (append begin end)))

;; Per the problem description, points a laser north and rotates it clockwise,
;; vaporizing one asteroid at any given angle per rotation. Returns a list of
;; zapped asteroids in the order they were zapped.
(define (zap-em-all asteroids origin)
  (let loop ([zapped '()]
             [remaining (set-subtract asteroids (list origin))])
    (cond [(empty? remaining) zapped]
          [else
           (define targets (visible-clockwise-from-north remaining origin))
           (define new-remaining (set-subtract remaining targets))
           (loop (append zapped targets) new-remaining)])))

(define (day10-p2)
  (match-define (cons origin _) (best-asteroid input))
  (define zaplist (zap-em-all input origin))
  (define luckyboi (list-ref zaplist 199))
  (+ (* (real-part luckyboi) 100) (imag-part luckyboi)))

(check-equal? (time (day10-p2)) 404) ;; Answer for part 2

(define tx
  (string->asteroid-pts
   ".#....#####...#..
##...##.#####..##
##...#...#.#####.
..#.....#...###..
..#.#.....#....##"))

(define t1
  (string->asteroid-pts
   ".#..#
.....
#####
....#
...##"))

(define t2
  (string->asteroid-pts
   "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####"))

(define t3
  (string->asteroid-pts
   "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###."))

(define t4
  (string->asteroid-pts
   ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#.."))

(define t5
  (string->asteroid-pts
   ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"))