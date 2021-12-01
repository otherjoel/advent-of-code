#lang racket

(require pict rackunit)

(struct point (coords speed) #:transparent)

;; Convert one line of the input to a point
(define (string->point str)
  (define str-nums
    (map string->number 
         (rest (regexp-match #px"position=<\\s*([-]*\\d+),\\s*([-]*\\d+)> velocity=<\\s*([-]*\\d+),\\s*([-]*\\d+)>" str))))
  (point (take str-nums 2) (drop str-nums 2)))

(define input (map string->point (file->lines "day10-input.txt")))
(define example (map string->point (file->lines "day10-example.txt")))

;; Move a point according to its velocity
(define (point-tick p)
  (match-define (point coords speeds) p)
  (point (map + coords speeds) speeds))

;; Returns the area of the smallest rectangular grid needed
;; to hold all the given points in their current positions
(define (bounding-square-area points)
  (match-define (list xs ys) (apply map list (map point-coords points)))
  (* (add1 (- (apply max xs) (apply min xs)))
     (add1 (- (apply max ys) (apply min ys)))))

;; Iterate to find the locations of all the points when they converge on the smallest area.
;; Returns the list point coordinates at such a state, as well as area of the bounding square
;; and the number of “seconds” (iterations) it took to get there.
(define (find-convergence starting-points)
  (define starting-size (bounding-square-area starting-points))
  (let loop ([last-points starting-points]
             [last-size starting-size]
             [seconds 0])
    (define new-points (map point-tick last-points))
    (define new-size (bounding-square-area new-points))
    (cond [(> new-size last-size) (values (map point-coords last-points) last-size seconds)]
          [else (loop new-points new-size (add1 seconds))])))

;; Translates a list of coordinates, making them relative to a (0,0) starting-point
(define (translate-coords coord-list)
  (match-define (list xs ys) (apply map list coord-list))
  (define min-x (apply min xs))
  (define min-y (apply min ys))
  (map (lambda (coord) (map - coord (list min-x min-y))) coord-list))

;; Returns a pict with a box drawn at each of the coordinates
(define (coords->pict coord-list)
  (match-define (list xs ys) (apply map list coord-list))
  (define max-x (apply max xs))
  (define max-y (apply max ys))
  (define pixel-size 10)
  (define (render-pixels dc dx dy)
    (send dc set-brush "orange" 'solid)
    (send dc set-pen "black" 1 'solid)
    (for ([c (in-list coord-list)])
         (match-define (list x y) c)
         (send dc draw-rectangle (* x pixel-size) (* y pixel-size) pixel-size pixel-size)))
  
  (unsafe-dc render-pixels (* pixel-size (add1 max-x)) (* pixel-size (add1 max-y))))

;; Part 1: What is the message eventually spelled out?
;; Use your eyeballs for this one.
(define (day10-part1)
  (define-values (coord-list size secs) (find-convergence input))
  (define result (coords->pict (translate-coords coord-list)))
  (send (pict->bitmap result) save-file "day10-output.png" 'png)
  result)

;; Part 2: How many “seconds” will it take for the message to be spelled out?
(define (day10-part2)
  (define-values (c sz secs) (find-convergence input))
  secs)

(module+ test
  (check-equal? (time (day10-part2)) 10605)) ; Correct answer for part 2
;; Did not bother writing a test for part 1