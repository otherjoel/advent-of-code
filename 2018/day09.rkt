#lang racket

(require rackunit sugar)

;; Parse the problem input
(define input
  (map string->number
       (rest (regexp-match #px"(\\d+) players; last marble is worth (\\d+) points"
                           (file->string "day09-input.txt")))))

(match-define (list input-num-players input-last-marble) input)

(struct zipper (head current tail) #:transparent)

;; Zipper movement, transparently wrapping around in circular fashion
(define (zipper-next z)
  (match-define (zipper h c t) z)
  (cond
    [(and (empty? t) (empty? h)) z]
    [(empty? t) (zipper (list c) (first (reverse h)) (rest (reverse h)))]
    [else (zipper (cons c h) (first t) (rest t))]))

(define (zipper-prev z)
  (match-define (zipper h c t) z)
  (cond
    [(and (empty? t) (empty? h)) z]
    [(empty? h) (zipper (rest (reverse t)) (first (reverse t)) (list c))]
    [else (zipper (rest h) (first h) (cons c t))]))

;; Change the value at the cursor location
(define (zipper-set z new-current)
  (match-define (zipper h _ t) z)
  (zipper h new-current t))

;; Applies pred to the current value
(define (zipper-edit z pred)
  (match-define (zipper h c t) z)
  (zipper h (pred c) t))

;; Remove the current value and move the cursor to the next
;; item in `tail`. Returns 2 values: the resulting zipper and the
;; removed value.
(define (zipper-remove z)
  (match-define (zipper h c t) z)
  (values
   (cond [(empty? t) (zipper '() (first (reverse h)) (rest (reverse h)))]
         [else (zipper h (first t) (rest t))])
   c))

;; Insert a value into the current position, pushing the
;; old current value to the left (onto the `head`)
(define (zipper-insert z val)
  (match-define (zipper h c t) z)
  (cond
    [(empty? h) (zipper (list c) val t)]
    [else (zipper (cons c h) val t)]))

;; Move the zipper a given number of times using dir
;; `dir` would normally be zipper-next or zipper-prev
;; but can also be zipper-remove or any function that
;; takes a zipper as its only argument
(define (move-along z dir #:times [times 1])
  (cond [(zero? times) z]
        [else (move-along (dir z) dir #:times (sub1 times))]))

(define (list->zipper lst)
  (zipper empty (first lst) (rest lst)))

(define (zipper->list z)
  (match-define (zipper h c t) z)
  (append (reverse h) (list c) t))

;; Now that we have zippers to play with, spell out the problem’s
;; functions with painful clarity

;; Insert a marble between the first and second marbles after the
;; current marble
(define (insert-marble z marble)
  (zipper-insert (zipper-next z) marble))

;; Remove the seventh marble and return `(values zipper removed-marble)`
(define (remove-7th-marble z)
  (zipper-remove (move-along z zipper-prev #:times 7)))

;; Returns a function that will add a single number to the 
(define (adder marble1 marble2)
  (curry + (* (+ marble1 marble2) (sgn marble2))))

;; Play the game with specified number of players and marbles.
;; Returns zippers for both the player scores and the resulting marble circle
(define (play-game num-players last-marble)
  (for/fold ([scores (list->zipper (make-list num-players 0))]
             [circle (list->zipper '(0))])
            ([current-marble (in-naturals 1)])
    #:final (equal? current-marble last-marble)
    (define-values (result-circle maybe-removed-marble)
      (cond [(zero? (modulo current-marble 23))
             (remove-7th-marble circle)]
            [else
             (values (insert-marble circle current-marble) 0)]))
    (define new-score (+ (zipper-current scores)
                         (* (+ current-marble maybe-removed-marble) (sgn maybe-removed-marble))))
    (values (zipper-next (zipper-set scores new-score)) result-circle)))

(define (day09-part1 [players input-num-players] [last-marble input-last-marble])
  (define-values (scores _) (play-game players last-marble))
  (apply max (zipper->list scores)))

(module+ test
  ;; Example inputs
  (check-equal? (day09-part1 10 1618) 8317)
  (check-equal? (day09-part1 13 7999) 146373)
  (check-equal? (day09-part1 17 1104) 2764)
  (check-equal? (day09-part1 21 6111) 54718)
  (check-equal? (day09-part1 30 5807) 37305)

  ;; Finishes in ≈40 msec (using `raco test`) or ≈120 msec (in DrRacket)
  (check-equal? (time (day09-part1)) 374287)) ; Correct answer for part 1

(define (day09-part2)
  (day09-part1 input-num-players (* 100 input-last-marble)))

(module+ test
  ;; Finishes in ≈7,500 msec (using `raco test`) or ≈18,400 msec (in DrRacket)
  (check-equal? (time (day09-part2)) 3083412635)) ; Correct answer for part 2