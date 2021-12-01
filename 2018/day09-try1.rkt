#lang racket

(require sugar rackunit)

(define input
  (map string->number
       (rest (regexp-match #px"(\\d+) players; last marble is worth (\\d+) points"
                           (file->string "day09-input.txt")))))

(match-define (list num-players last-marble-pts) input)

(define circle (list 0))

;; Note these number refers to the “nth“ spots in their lists;
;; they are not zero-based!
(define cur-marble 1)
(define cur-player 1)

(define players (make-list num-players 0))

(define (set-next-player!)
  (define n (add1 cur-player))
  (set! cur-player (if (> n (length players)) (- n (length players)) n)))

(define (add-to-current-player-score! score)
  (define new-score (+ score (list-ref players (sub1 cur-player))))
  (set! players (list-set players (sub1 cur-player) new-score)))

;; Inserts an item into the `circle` list after the element specified
(define (insert-into-circle! val after-nth)
  (set! circle (append (take circle after-nth)
                       (list val)
                       (drop circle after-nth)))
  (set! cur-marble (add1 after-nth)))

;; Returns the count of the element in `circle` after which the
;; next marble should be placed
(define (next-marble-after-nth)
  (define n (add1 cur-marble))
  (cond [(> n (length circle)) (- n (length circle))]
        [else n]))

(define (remove-nth-marble! n)
  (set! circle (append (take circle (sub1 n))
                       (drop circle n)))
  (set! cur-marble n))

(define (score! marble-val)
  (define 7th-marble-clockwise
    (cond [(> cur-marble 7) (- cur-marble 7)]
          [else (+ (length circle) (- cur-marble 7))]))
  (define 7th-marble-value (list-ref circle (sub1 7th-marble-clockwise)))
  (define turn-score (+ marble-val 7th-marble-value))
  (add-to-current-player-score! turn-score)
  (set-next-player!)
  (remove-nth-marble! 7th-marble-clockwise))

;; Place a marble in the correct spot
(define (place-marble! new-marble-val)
  (cond [(zero? (modulo new-marble-val 23))
         (score! new-marble-val)]
        [else
         (define after-nth (next-marble-after-nth))
         (insert-into-circle! new-marble-val after-nth)]))

(define (test n)
  (for ([i (in-range n)])
        (place-marble! (add1 i))))

;; Gets me the correct answer (374287)
(define (day09-part1)
  (for ([i (in-range last-marble-pts)])
       (place-marble! (add1 i)))
  (apply max players))


;; I left this running overnight and it did not finish!
(define (day09-part2)
  (set! last-marble-pts (* 100 last-marble-pts))
  (day09-part1))