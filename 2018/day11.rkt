#lang racket

(require rackunit sugar/debug)

;; Since the input is just a single number, we’ll make it a parameter.
(define input (make-parameter 9424)) ; = my input
(define square-size (make-parameter 3))

;; Return the hundreds digit of a number if there is one, or zero if there isn’t
(define (hundreds-digit n)
  (cond [(< n 100) 0]
        [else (let* ([str (number->string n)]
                     [strlen (string-length str)]
                     [start (- strlen 3)])
                (string->number (substring str start (+ 1 start))))]))

;; Calculates the power at the given coordinates
(define (power-at x y)
  (let* ([rack-id (+ x 10)]
         [powerlevel (+ (input) (* rack-id y))]
         [powerlevel (* powerlevel rack-id)]
         [powerlevel (hundreds-digit powerlevel)])
    (- powerlevel 5)))

;; Make sure we’re getting values that match the examples
(module+ test
  (parameterize ([input 8])
    (check-equal? (power-at 3 5) 4))
  (parameterize ([input 57])
    (check-equal? (power-at 122 79) -5))
  (parameterize ([input 39])
    (check-equal? (power-at 217 196) 0))
  (parameterize ([input 71])
    (check-equal? (power-at 101 153) 4)))

;; I have a notion that numeric keys for hash tables are faster somehow
(define (grid-key x y)
  (+ (* 1000 x) y))

;; If either coord is zero, returns 0
(define (ref-z h x y)
  (cond [(zero? (* x y)) 0]
        [else (hash-ref h (grid-key x y))]))
         
;; Returns a summed-area hash table
(define (summed-area-grid)
  (for*/fold ([table (hash)])
             ([x (in-range 1 301)]
              [y (in-range 1 301)])
    (define cur (- (+ (power-at x y)
                      (ref-z table x (sub1 y))
                      (ref-z table (sub1 x) y))
                   (ref-z table (sub1 x) (sub1 y))))
    (hash-set table (grid-key x y) cur)))

;; Return the total power-level of the square of the grid starting at x,y
(define (square-total-power grid x y)
  (- (+ (ref-z grid (sub1 x) (sub1 y))
        (hash-ref grid (grid-key (+ x (sub1 (square-size)))
                                 (+ y (sub1 (square-size))))))
     (ref-z grid (sub1 x) (+ y (sub1 (square-size))))
     (ref-z grid (+ x (sub1 (square-size))) (sub1 y))))

;; Check the sums against the examples
(module+ test
  (parameterize ([input 18])
    (check-equal? (square-total-power (summed-area-grid) 33 45) 29))
  (parameterize ([input 42])
    (check-equal? (square-total-power (summed-area-grid) 21 61) 30)))

;; Returns a list of the power levels for all the NxN squares in the grid
;; Each item in the list identifies the power level, coords, and square size
(define (square-power-levels grid)
  (define end-before (- 300 (max 0 (- (square-size) 2))))
  (for*/list ([x (in-range 1 end-before)]
              [y (in-range 1 end-before)])
             (list (square-total-power grid x y) x y (square-size))))

;; Return the power-levl and coordinates of the upper left cell in the
;; 3x3 quadrant with the highest total power
(define (day11-part1 grid)
  (first (sort (square-power-levels grid) > #:key car)))

;; Test with examples, then for real
(module+ test
  (parameterize ([input 18])
    (check-equal? (day11-part1 (summed-area-grid)) '(29 33 45 3)))
  (parameterize ([input 42])
    (check-equal? (day11-part1 (summed-area-grid)) '(30 21 61 3)))
  
  (check-equal? (report (time (day11-part1 (summed-area-grid))))
                '(28 243 72 3))) ; 243,72 = correct answer for part 1

;; Test all square sizes from 1 to 300, find the coordinates and size of
;; the square with the highest total power
(define (day11-part2)
  (define grid (summed-area-grid))
  (define candidates
    (for/list ([i (in-range 1 301)])
              (parameterize ([square-size i])
                (day11-part1 grid))))
  (first (sort candidates > #:key car)))

(module+ test
  (check-equal? (report (time (day11-part2))) '(74 229 192 11))) ; 229,192,11 = correct answer for part 2