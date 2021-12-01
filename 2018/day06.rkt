#lang racket

(require rackunit)

(define (inputline->pair str)
  (map string->number (string-split str ", ")))

(define input (map inputline->pair (file->lines "day06-input.txt")))

;; Get the biggest coordinate and use that as the grid size in both directions
(define grid-size (apply max (first (sort input > #:key (curry apply max)))))

;; Manhattan distance is just the sum of the diff between two x and y coordinates
(define (distance-between pt1 pt2)
  (+ (abs (- (first pt1) (first pt2)))
     (abs (- (second pt1) (second pt2)))))

;; Return the index of "input" that is the closest point to the point given,
;; or -1 if there is a tie
(define (closest-input-pt p)
  (define distances (map (curry distance-between p) input))
  (define closest-idxs (indexes-of distances (apply min distances)))
  (cond
    [(> (length closest-idxs) 1) -1]
    [else (first closest-idxs)]))

;; Create a list of all x,y points in the grid, together with a third element:
;; the index of the closest point in `inputs` (-1 if a tie)
(define grid
  (for*/list ([x (in-range grid-size)]
              [y (in-range grid-size)])
             (list x y (closest-input-pt (list x y)))))

; An edge point is one where either coordinate is 0 or equal to `grid-size`
(define (is-edge? pt)
  (let ([coords (take pt 2)])
    (or (member 0 coords) (member (sub1 grid-size) coords))))

;; Identify the indexes of the points in `input` whose areas are infinite
;; I assume the "infinite" areas are those which touch the grid edges
(define infinite-area-pt-idxs
  (let ([edges (filter is-edge? grid)])
    ; don't include -1 in result
    (remove-duplicates (filter positive? (map third edges)))))

;; A list of same length as `input`, where each element is the size of the area
;; of the corresponding point in `input`
(define point-areas
  (for/list ([idx (in-range (length input))])
            (count (curry = idx) (map third grid))))

(define (day06-part1)
  (define filtered-pt-areas
    (filter positive?
            (for/list ([idx (in-range (length input))])
                      (if (member idx infinite-area-pt-idxs) 0 (list-ref point-areas idx)))))
  (apply max filtered-pt-areas))

(module+ test
  (check-equal? (time (day06-part1)) 5626)) ; Correct answer for part 1

(define (sum-of-distances grid-pt)
  (apply + (map (curry distance-between (take grid-pt 2)) input)))

(define (day06-part2)
  (count (curryr < 10000) (map sum-of-distances grid)))

(module+ test
  (check-equal? (time (day06-part2)) 46554)) ; Correct answer for part 2