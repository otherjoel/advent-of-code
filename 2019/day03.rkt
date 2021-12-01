#lang racket/base

(require racket/match
         racket/list
         racket/file
         racket/set
         threading)

(define (string->moves str)
  (map (lambda (x) (list (first x) (string->number (second x))))
       (regexp-match* #px"([A-Z])(\\d+)" str #:match-select cdr)))

(define (manhattan-distance pt)
  (match-define (vector x y) pt)
  (+ (abs x) (abs y)))

(define (next-point cur dir)
  (match-define (vector x y) cur)
  (case dir
    [("U") (vector x (- y 1))]
    [("D") (vector x (+ y 1))]
    [("L") (vector (- x 1) y)]
    [("R") (vector (+ x 1) y)]))

(define (wire-points moves)
  (for*/fold ([points '()]
              #:result (reverse points))
             ([move (in-list moves)]
              [n (in-range (second move))])
    (cons (next-point (if (null? points) '#(0 0) (car points))
                      (first move))
          points)))

;; TIL: set-intersect is vastly slower on normal lists
;; Convert them to sets first for the big win!
(define (find-crossings pts1 pts2)
  (set->list (set-intersect (list->set pts1) (list->set pts2))))

(define (nearest-crossing str1 str2)
  (define w1 (wire-points (string->moves str1)))
  (define w2 (wire-points (string->moves str2)))
  (~>> (find-crossings w1 w2)
       (map manhattan-distance)
       (sort _ <)
       first))

(module+ test
  (require rackunit)
  (check-equal? (nearest-crossing "R8,U5,L5,D3"
                                  "U7,R6,D4,L4") 6)
  (check-equal? (nearest-crossing "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                  "U62,R66,U55,R34,D71,R55,D58,R83") 159)
  (check-equal? (nearest-crossing "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                                  "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") 135))

(define input (file->lines "day03-input.txt"))

(define (day3-p1)
  (nearest-crossing (first input) (second input)))

(module+ test
  ; cpu time: 1281 real time: 1300 gc time: 765
  (check-equal? (time (day3-p1)) 303)) ; correct part 1 answer for my input
 
(define (earliest-crossing-steps str1 str2)
  (define w1 (wire-points (string->moves str1)))
  (define w2 (wire-points (string->moves str2)))
  (apply min
         (for/list ([c (in-list (find-crossings w1 w2))])
           (+ (add1 (index-of w1 c)) (add1 (index-of w2 c))))))

(module+ test
  (check-equal? (earliest-crossing-steps "R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                         "U62,R66,U55,R34,D71,R55,D58,R83") 610)
  (check-equal? (earliest-crossing-steps "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                                         "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") 410))

(define (day3-p2)
  (earliest-crossing-steps (first input) (second input)))

(module+ test
  ; cpu time: 2781 real time: 2835 gc time: 718
  (check-equal? (time (day3-p2)) 11222)) ; correct part 2 answer for my input