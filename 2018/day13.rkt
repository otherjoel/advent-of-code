#lang racket

(require rackunit sugar/list sugar/debug)

(define (load-tracks filename)
  (for/hash ([i (in-naturals)]
             [line (in-list (file->lines filename))])
    (values i (string->list line))))
  

(define input (load-tracks "day13-input.txt"))
(define example (load-tracks "day13-example.txt"))

;; I define a DIRECTION in terms of its effect on the x,y coordinates.
;; And a COMPASS is a list of DIRECTIONs, with current direction in 1st position.
;; Rotate the list one way or the other == turning direction.
;; Listed here starting with UP, moving counter-clockwise
(define cardinal-directions '((0 -1) (-1 0) (0 1) (1 0)))

;; Useful aliases
(define go-left (curryr shift-left-cycle 1))
(define go-straight identity) ; RIP my mentions
(define go-right (curryr shift-cycle 1))

;; A cart has an (x y) position, a “compass” which is a rotating list of an (x y) direction,
;; and a next-turn (the first in a cycling list of functions)
(struct cart (pos compass next-turn) #:transparent)

;; Map characters in the input
(define CART-DIRS (hash #\^ '(0 -1) #\> '(1 0) #\v '(0 1) #\< '(-1 0)))
(define CART-TRACKS (hash #\^ #\| #\> #\- #\v #\| #\< #\-))

(define TURN-HANDS (list go-left go-straight go-right))

(define corner-turns
  (hash '(#\\ (0 -1)) go-left
        '(#\\ (0 1))  go-left
        '(#\\ (-1 0)) go-right
        '(#\\ (1 0))  go-right
        '(#\/ (0 -1)) go-right
        '(#\/ (0 1))  go-right
        '(#\/ (-1 0)) go-left
        '(#\/ (1 0))  go-left))

;; Given a character representing a cart, return a
;; compass oriented in that direction
(define (init-compass cart-char)
  (shift-left-cycle
   cardinal-directions
   (index-of cardinal-directions (hash-ref CART-DIRS cart-char))))

;; Construct a cart given its position and a map of the tracks
(define (make-cart posn tracks)
  (match-define (list x y) posn)
  (define cart-char (list-ref (hash-ref tracks y) x))
  (cart posn (init-compass cart-char) TURN-HANDS))

;; Returns the symbol for the tracks at x y
;; If a cart symbol is there, return the symbol of the tracks that are underneath it
(define (track-at tracks posn)
  (match-define (list x y) posn)
  (define char (list-ref (hash-ref tracks y) x))
  (cond [(equal? char #\space) (error 'track-at "cart off the rails? ~a,~a" x y)])
  (hash-ref CART-TRACKS char char))

;; Given a track character and a compass, return a function that will rotate the
;; compass in the proper direction. Does not handle intersections though!
(define (new-direction track-char compass)
  (cond [(member track-char '(#\| #\-)) go-straight]
        [else (hash-ref corner-turns (list track-char (first compass)))]))

;; Move a cart one coordinate in its current direction and turn appropriately
;; for the tracks encountered at the new location
(define (tick/move c tracks)
  (match-define (cart coords compass turn-hands) c)
  (define new-coords (map + coords (first compass)))
  (define new-track (track-at tracks new-coords))
  (define-values (turn-func new-turn-hands)
    (cond [(equal? new-track #\+) (values (first turn-hands) (shift-left-cycle turn-hands 1))]
          [else (values (new-direction new-track compass) turn-hands)]))
  (cart new-coords (turn-func compass) new-turn-hands))

(define (is-cart-char? c)
  (member c (hash-keys CART-DIRS)))

;; Given a row and a y-coordinate, return a list of (x y) for each cart
(define (cart-coords-in-row tracks y-coord)
  (define clist (hash-ref tracks y-coord))
  (map (curryr list y-coord) (indexes-where clist is-cart-char?)))

;; Builds and returns a list of all carts on the tracks
(define (all-carts-on-tracks tracks)
  (for/fold ([carts '()])
            ([y (in-range (length (hash-keys tracks)))])
    (append carts
            (for/list ([coord (in-list (cart-coords-in-row tracks y))])
              (make-cart coord tracks)))))

;; Comparison function for sorting a list of carts
;; Ensures sorted list shows carts in top-down, left-to-right
;; order based on their x,y coordinates
(define (cart<? a b)
  (match-define (list (list a-x a-y) (list b-x b-y)) (map cart-pos (list a b)))
  (or (< a-y b-y)
      (and (equal? a-y b-y) (< a-x b-x))))

;; Returns (x y) coordinates of the first collision
;; Note that this function gets the correct answer but does not correctly handle
;; the case of carts that begin a tick adjacent to and facing each other
;; (see function `move-all` below)
(define (day13-part1 tracks)
  (let loop ([carts (all-carts-on-tracks tracks)])
    (define maybe-collision (check-duplicates carts #:key cart-pos))
    (cond [maybe-collision (cart-pos maybe-collision)]
          [else
           (define next-carts
             (for/list ([c (in-list (sort carts cart<?))])
               (tick/move c tracks)))
           (loop next-carts)])))

(module+ test
  (check-equal? (day13-part1 input) '(91 69))) ; Correct answer for part 1

(define (cart=? a b)
  (equal? (cart-pos a) (cart-pos b)))

;; Removes any collided carts from a list of carts
(define (remove-collisions carts)
  (define collision (check-duplicates carts #:key cart-pos))
  (cond [(not collision) carts]
        [else
         (report collision) ; Debugging
         (remove-collisions (filter-not (curry cart=? collision) carts))]))


;; (Debugging instrumentation)
;; List all carts whose x OR y coordinates differ by only 1
(define (adjacents carts)
  (define (adjacent? a b)
    (match-define (list a-x a-y) (cart-pos a))
    (match-define (list b-x b-y) (cart-pos b))
    (or (and (equal? a-x b-x) (equal? 1 (abs (- a-y b-y))))
        (and (equal? a-y b-y) (equal? 1 (abs (- a-x b-x))))))
  (for*/fold ([adjacent-accum '()]
              #:result adjacent-accum)
             ([a (in-list carts)]
              [b (in-list carts)])
    (cond [(adjacent? a b) (cons a adjacent-accum)]
          [else adjacent-accum])))

;; (Debugging instrumentation)
(define (display-cart c)
  (match-define (cart pos compass turn-hands) c)
  (define chrs (hash '(0 -1) "^" '(-1 0) "<" '(0 1) "v" '(1 0) ">"))
  (format "~a ~a" pos (hash-ref chrs (first compass))))

;; (Debugging instrumentation)
;; Save a snapshot of the tracks depicting cart locations/directions to day13-output.txt
(define (save-tracks tracks now-carts)
  (define chrs (hash '(0 -1) #\^ '(-1 0) #\< '(0 1) #\v '(1 0) #\>))
  (define track-cleanup
    (for/fold ([track-changes tracks])
              ([c (in-list (all-carts-on-tracks tracks))])
      (match-define (cart coord compass turns) c)
      (define old-row (hash-ref track-changes (second coord)))
      (hash-set track-changes (second coord) (list-set old-row (first coord) (track-at tracks coord)))))
  (define track-updated
    (for/fold ([track-changes track-cleanup])
              ([c (in-list now-carts)])
      (match-define (cart coord compass turns) c)
      (define old-row (hash-ref track-changes (second coord)))
      (hash-set track-changes (second coord) (list-set old-row (first coord) (hash-ref chrs (first compass))))))
  (define map-lines
    (for/list ([i (in-range (apply max (hash-keys track-updated)))])
      (list->string (hash-ref track-updated i))))
  (display-lines-to-file map-lines "day13-output.txt" #:exists 'replace)
  now-carts)

;; Moves the carts one at a time, in order (left to right, top to bottom),
;; removing collisions as they occur, returning the list of surviving carts.
(define (move-all carts-remaining tracks [carts-moved '()])
  (cond
    [(empty? carts-remaining) carts-moved]
    [else
     (match-define (cons this-cart rest-carts) (sort carts-remaining cart<?))
     (define cart-moved (tick/move this-cart tracks))
     (cond [(member (cart-pos cart-moved) (map cart-pos carts-moved))
            (move-all rest-carts tracks (remove-collisions (cons cart-moved carts-moved)))]
           [(member (cart-pos cart-moved) (map cart-pos rest-carts))
            (move-all (remove-collisions (cons cart-moved rest-carts)) tracks carts-moved)]
           [else (move-all rest-carts tracks (cons cart-moved carts-moved))])]))

;; Find the coordinates of the last surviving cart at the end of the first tick
;; where all the others have perished
(define (day13-part2 tracks [limit -1])
  (let loop ([current-carts (all-carts-on-tracks tracks)]
             [secs 0])
    (display (format "~a sec: ~a carts\n" secs (length current-carts)))
    (cond [(equal? 1 (length current-carts)) (first current-carts)]
          [(equal? secs limit) current-carts]
          [else
           (loop (move-all current-carts tracks) (add1 secs))])))

(module+ test
  (check-equal? (cart-pos (day13-part2 input)) '(44 87))) ; Correct answer for part 2

;; (Debugging instrumentation)
;; Replay the cart movements up to n ticks and save a snapshot to day13-output.txt
(define (saveat n)
  (define a (day13-part2 input n))
  (save-tracks input a))