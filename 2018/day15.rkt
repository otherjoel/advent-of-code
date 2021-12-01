#lang debug racket/base

(require racket/match
         racket/function
         racket/format
         racket/list
         racket/vector
         racket/set
         racket/file
         threading)

;; PRIMITIVES AND CONSTANTS ----------------------------------------------------

;; Select the “lowest” somethings from a list, with “lowest what” being determined
;; by the key-procedure.
(define (select-minimums lst key-pred)
  (define minval (apply min (map key-pred lst)))
  (filter (lambda (x) (equal? minval (key-pred x))) lst))

;; Good ol’ positions.
(struct posn (x y) #:transparent #:mutable)

;; The concept of “reading order” is an important one in this puzzle. Fighters move,
;; targets and paths are chosen in order of how you’d encounter them reading the grid
;; top to bottom, left-to-right.
;;   The two functions below are going to do all the work of determining sort order
;; for us, whenever we need it.
;;   This is also where I mention that this program views 0,0 as “top left”.
(define (posn<? p1 p2)
  (match-define (posn x1 y1) p1)
  (match-define (posn x2 y2) p2)
  (or (< y1 y2)
      (and (<= y1 y2)
           (<= x1 x2))))

(define (reading-order lst)
  (sort lst posn<?))

(define (first-by-reading-order lst)
  (first (sort lst posn<?)))

(define (posn=? p1 p2)
  (and (equal? (posn-x p1) (posn-x p2))
       (equal? (posn-y p1) (posn-y p2))))

;; Keeping track of elves and gnomes. We’ll have separate lists for each group.
;; Making this a subtype of posn means we can pass a fighter to any function
;; that expects a posn.
(struct fighter posn (type hp) #:transparent #:mutable)
(define ATTACK-POWER 3)
(define STARTING-HP 200)

;; Paths are also a subtype of posn. The path’s posn elements reflect the
;; intended end-points of the path. This will allow us to sort lists of
;; paths using reading-order. We don’t need to carry the complete list of
;; steps in the path, just the candidates for first steps along it.
(struct path (distance first-steps) #:super struct:posn #:transparent)

;; “The grid…a digital frontier. I tried to picture clusters of information as
;; they moved through the computer. What did they look like? …I kept dreaming
;; of a world I thought I’d never see. And then one day…I got in.”
;;   https://youtu.be/QBYr0k8dOtw?t=24
(struct grid (vec rows cols) #:transparent)

(define EMPTY #\.)

;; Our “grid” is, behind the scenes, a one-dimensional vector with length ROWS*COLS.
;; So we’ll need to translate between an x,y pair of coordinates and an index into
;; the grid vector
(define (posn→index g p)
  (+ (* (grid-cols g) (posn-y p)) (posn-x p)))

(define (index→posn g i)
  (posn (modulo i (grid-cols g))
        (quotient i (grid-cols g))))

;; WORKING WITH GRIDS ----------------------------------------------------------

;; Create a grid from a list of strings each representing a row, filling each
;; spot with the corresponding character in the string
(define (lines→grid line-strs)
  (define row-count (length line-strs))
  (define col-count (string-length (first line-strs)))
  (grid (apply vector-append
               (map list->vector
                    (map string->list line-strs)))
        row-count
        col-count))

;; Reference the value at given position in a grid
(define (grid-ref g p)
  (vector-ref (grid-vec g) (posn→index g p)))

;; Change the value at given position
(define (grid-mark! g pos v)
  (vector-set! (grid-vec g) (posn→index g pos) v))

;; Used to determine if a fighter could move into a given spot.
;; Anything besides "." counts as an obstruction (incl. other fighters)
(define (grid-clear-at? g p)
  (equal? (grid-ref g p) EMPTY))

;; Make a blank grid of the same dimensions, for use in making “path grids” (see
;; further below)
(define (copy-blank-grid g)
  (match-define (grid _ rows cols) g)
  (grid (make-vector (* rows cols) #f) rows cols))

;; (For debugging) Represent the grid as a square of single-character values
(define (display-grid g [g2 #f])
  (define grid-size (* (grid-cols g) (grid-rows g)))
  (define strs
    (append (list "   ")
            (for/list ([i (in-range (grid-cols g))]) ;; column guide
              (number->string (modulo i 10)))
            (list "\n")
            (for/fold ([lst (list "00 ")]
                       #:result (reverse (cons "\n" lst)))
                      ([val (in-vector (grid-vec g))]
                       [i (in-naturals 1)])
              (define ch
                (cond [(number? val) (number->string (modulo val 10))]
                      [(boolean? val) "-"]
                      [(string? val) val]
                      [else (format "~a" val)]))
              (cond [(and (equal? 0 (modulo i (grid-cols g)))
                          (< i grid-size))
                     (cons (format "\n~a " (~r (quotient i (grid-cols g)) #:min-width 2 #:pad-string "0"))
                           (cons ch lst))]
                    [else (cons ch lst)]))))
  (display (apply string-append strs)))

;; Is point p inside grid g? Film at 11
(define (inside-grid? g p)
  (match-define (posn px py) p)
  (and (>= px 0)
       (>= py 0)
       (< px (grid-cols g))
       (< py (grid-rows g))))

;; Get a list of a positions neighboring points, ensuring none are out of bounds
(define (neighbor-coords g pos)
  (match-define (posn x y) pos)
  (filter (curry inside-grid? g)
          (list (posn (- x 1) y)
                (posn x (+ y 1))
                (posn (+ x 1) y)
                (posn x (- y 1)))))

;; Get all the EMPTY neighboring points of a given spot OR list of spots.
;; If a (listof posn?) is passed, ensures the returned list does not include
;; any of the original positions.
(define (free-neighbors-of world pos)
  (cond [(posn? pos)
         (~> (neighbor-coords world pos)
             (filter (curry grid-clear-at? world) _))]
        [(list? pos)
         (~> (map (curry neighbor-coords world) pos)
             flatten
             (filter (curry grid-clear-at? world) _)
             (set-subtract pos)
             remove-duplicates)]))

;; Working with PATHS ----------------------------------------------------------

;; Find the most direct path(s) to a fighter from an end-position.
;; This is the function you are probably looking for if you are reading this file at all.
;; The algorithm starts at the end position and works outwards, finding unoccupied positions
;; and marking them (on a blank copy of the map) with their distance from the end-point.
;; As soon as any of the considered points includes one or more free neighbors of the given
;; fighter, recursion stops and returns a path.
(define (build-path world f end-pos)
  (define result-grid (copy-blank-grid world))
  (define (not-yet-checked? pos) (not (grid-ref result-grid pos)))
  (define goal-pts (free-neighbors-of world f))
  
  (cond
    [(member end-pos goal-pts)
     (path (posn-x end-pos) (posn-y end-pos) 0 (list end-pos))]
    [else
     (grid-mark! result-grid end-pos 0)
     (let loop ([pts-to-check (list end-pos)]
                [i 1])
       (define new-coords (~> (free-neighbors-of world pts-to-check)
                              (filter not-yet-checked? _)))
       (define maybe-first-steps (set-intersect new-coords goal-pts))
       (cond
         [(not (empty? maybe-first-steps))
          (display-grid result-grid)
          (path (posn-x end-pos) (posn-y end-pos) i maybe-first-steps)]
         [(empty? new-coords) #f]
         [else
          (for-each (lambda (p) (grid-mark! result-grid p i)) new-coords)
          (loop new-coords (+ 1 i))]))]))

(define (reachable-paths-to world f targets)
  (filter-map (curry build-path world f) targets))

;; Working with FIGHTERS -------------------------------------------------------

;; Let’s start doing stuff with fighters

;; Make a list of fighters from a grid, with the results in reading order.
(define (grid→fighters g)
  (for/fold ([fighters '()]
             #:result (reading-order fighters))
            ([val (in-vector (grid-vec g))]
             [idx (in-naturals)])
    (cond [(member val '(#\G #\E))
           (match-define (posn x y) (index→posn g idx))
           (cons (fighter x y val STARTING-HP) fighters)]
          [else fighters])))

;; I’ll give you three guesses each what these do
(define (fighter-located-in? f posns)
  (not (empty? (filter (curry posn=? f) posns))))

(define (enemies? f1 f2)
  (and (alive? f2)
       (not (equal? (fighter-type f1) (fighter-type f2)))))

(define (enemies-of f1 flst)
  (filter (curry enemies? f1) flst))

(define (adjacent-enemies world f all-enemies)
  (define adjacent-posns (neighbor-coords world f))
  (filter (curryr fighter-located-in? adjacent-posns) all-enemies))

(define (alive? f)
  (positive? (fighter-hp f)))

(define (move-fighter! world f to-pos)
  (match-define (posn new-x new-y) to-pos)
  (grid-mark! world f EMPTY)
  (grid-mark! world to-pos (fighter-type f))
  (set-posn-x! f new-x)
  (set-posn-y! f new-y))

;; If the attack proves fatal, return the killed fighter so the grid can be
;; updated.
(define (attack! victim)
  (define new-hp (- (fighter-hp victim) ATTACK-POWER))
  (set-fighter-hp! victim new-hp)
  (cond [(<= new-hp 0) victim]
        [else #f]))

;; The OVERALL LOGIC of the fight ----------------------------------------------

;; Taking a turn:
;; “If the unit is already in range of a target, it does not move, but continues
;; its turn with an ATTACK. Otherwise, since it is not in range of a target, it
;; MOVES. To MOVE the unit must:
;;   1) “Consider the squares that are in range [of a target]”
;;   2) “Determine which of those squares it could reach in the fewest steps.”
;;      • “If the unit cannot reach (find an open path to) any of the squares
;         that are in range, it ends its turn.”
;;      • “If multiple squares are in range and tied for being reachable in the
;;        fewest steps, the square which is first in reading order is chosen.”
;;   3) “Take a single step toward the chosen square along the shortest path to
;;      that square.”
;;      • “If multiple steps would put the unit equally closer to its destination,
;;        the unit chooses the step which is first in reading order.”
;; To ATTACK a unit must:
;;   1) “Determine all of the targets that are in range of it by being immediately
;;      adjacent to it.”
;;      • “If there are no such targets, the unit ends its turn.”
;;      • “Otherwise, the adjacent target with the fewest hit points is selected;
;;        in a tie, the adjacent target with the fewest hit points which is first
;;        in reading order is selected.”
;;    2) “The unit deals damage equal to its attack power to the selected target,
;;       reducing its hit points by that amount. If this reduces its hit points
;;       to 0 or fewer, the selected target dies.”
(define (fighter-take-turn! world f enemies)
  (unless (not (empty? (adjacent-enemies world f enemies)))
    (define viable-paths (~> (free-neighbors-of world enemies)
                             (reachable-paths-to world f _)))
    (when (not (empty? viable-paths))
      (define next-step (~> (select-minimums viable-paths path-distance)
                            first-by-reading-order
                            path-first-steps
                            first-by-reading-order))
      (move-fighter! world f next-step)))
  (define attackable-foes (adjacent-enemies world f enemies))
  (when (not (empty? attackable-foes))
    (define fighter-killed
      (~> (select-minimums attackable-foes fighter-hp)
          reading-order
          first
          attack!))
    (when fighter-killed (grid-mark! world fighter-killed EMPTY))))

(define (do-round world fighters)
  (for/and ([f (in-list (reading-order fighters))]) ; will break on first #f result
    (cond [(not (alive? f)) #t] ; silently skip anyone who died this round
          [else
           (define enemies (enemies-of f fighters))
           (cond [(not (empty? enemies))
                  (fighter-take-turn! world f enemies)
                  #t]
                 [else #f])])))

(define (FIGHT!! input-grid)
  (define initial-fighters (grid→fighters input-grid))
  (let another-round ([completed-rounds 0]
                      [fighters initial-fighters])
    ;#R completed-rounds
    ;#R (reading-order fighters)
    ;(display-grid input-grid fighters)
      
    (cond [(do-round input-grid fighters)
           (another-round (+ completed-rounds 1)
                          (filter alive? fighters))]
          [else (values completed-rounds (filter alive? fighters))])))

(define (total-hp fighters)
  (apply + (map fighter-hp fighters)))

(define (day15-part1 input-grid)
  (define-values (rounds survivors) (FIGHT!! input-grid))
  (apply * rounds (map fighter-hp survivors)))

(module+ test
  (require rackunit)
  ;; Three “edge cases” from
  ;; https://www.reddit.com/r/adventofcode/comments/a6rhzw/help_need_help_with_day_15_part_1/ebxk3v5
  (define test-g1
    (lines→grid '("#######"
                  "#.E..G#"
                  "#.#####"
                  "#G#####"
                  "#######")))
  (define fs-1 (reading-order (grid→fighters test-g1)))
  (define elf-1 (first fs-1))
  ;; “In this first case, the Elf should move to the right.”
  (fighter-take-turn! test-g1 elf-1 (enemies-of elf-1 fs-1))
  (check-true (posn=? elf-1 (posn 3 1)))

  (define test-g2
    (lines→grid '("####"
                  "#GG#"
                  "#.E#"
                  "####")))
  (define fs-2 (reading-order (grid→fighters test-g2)))
  (define elf-2 (third fs-2))
  ;; With this input, the elf should begin by attacking the goblin directly above him.
  (fighter-take-turn! test-g2 elf-2 (enemies-of elf-2 fs-2))
  (check-equal? (- STARTING-HP ATTACK-POWER) (fighter-hp (second fs-2)))

  (define test-g3
    (lines→grid '("########"
                  "#..E..G#"
                  "#G######"
                  "########")))
  (define fs-3 (reading-order (grid→fighters test-g3)))
  (define elf-3 (first fs-3))
  ;; “For this input, the elf should move to the left.”
  (fighter-take-turn! test-g3 elf-3 (enemies-of elf-3 fs-3))
  (check-true (posn=? elf-3 (posn 2 1)))

  ;; The last test case from the main puzzle page
  (define test-map
    (lines→grid
     '("#########"
       "#G......#"
       "#.E.#...#"
       "#..##..G#"
       "#...##..#"
       "#...#...#"
       "#.G...G.#"
       "#.....G.#"
       "#########" )))
  (define-values (rounds fighters) (FIGHT!! test-map))
  (check-equal? rounds 20)

  #;(define actual-input
      (lines→grid (file->lines "day15-input.txt")))
  ;(display-grid actual-input)
  ;(grid→fighters actual-input)
  #;(check-equal? (day15-part1 actual-input) 1))

(define g1 (lines→grid (file->lines "day15-input.txt")))
(define fs (reading-order (grid→fighters g1)))
(display-grid g1)