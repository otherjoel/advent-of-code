#lang racket

(require rackunit)

(define input
  (map string->number
       (rest (regexp-match #px"(\\d+) players; last marble is worth (\\d+) points"
                           (file->string "day09-input.txt")))))

(match-define (list num-players last-marble-pts) input)

;; In this variation, the circle is stored as a list. The “current”
;; marble is always the first item in the list.
(define (insert-and-rotate circle val after-nth)
  (values (append (list val)
                  (drop circle (min after-nth (length circle)))
                  (take circle (min after-nth (length circle)))) 0))

;; Remove the marble in the Nth slot, rotating the “circle” so the marble
;; to the right of the removed one is now in the front.
;; Returns both the rotated list and the value of the removed marble
(define (remove-and-rotate circle nth)
  (values (append (drop circle nth) (take circle (sub1 nth))) (list-ref circle (sub1 nth))))

(define (add-to-spot lst pos val)
  (append (take lst pos)
          (list (+ val (list-ref lst pos)))
          (drop lst (add1 pos))))

(define (play-game num-players until-marble)
  (for/fold ([scores (make-list num-players 0)]
             [circle (list 0)])
            ([player (in-cycle (range num-players))]
             [cur-marble (in-naturals 1)])
    #:final (equal? cur-marble until-marble)
    (define-values (result-circle turn-score)
      (cond [(zero? (modulo cur-marble 23))
             (remove-and-rotate circle (- (length circle) 6))]
            [else
             (insert-and-rotate circle cur-marble 2)]))
    (values (add-to-spot scores player (* (+ cur-marble turn-score) (sgn turn-score))) result-circle)))

(define (high-score num-players until-marble)
  (let-values ([(scores _) (play-game num-players until-marble)])
    (apply max scores)))

; Finishes in 78 seconds on a 2015 Apple rMBP
(define (day09-part1)
  (high-score num-players last-marble-pts))

(module+ test
  ;(check-equal? (high-score 10 1618) 8317)
  ;(check-equal? (high-score 13 7999) 146373)
  ;(check-equal? (high-score 17 1104) 2764)
  ;(check-equal? (high-score 21 6111) 54718)
  ;(check-equal? (high-score 30 5807) 37305)

  (check-equal? (time (day09-part1)) 374287)) ; Correct answer for part 1

; Ran overnight — still didn’t finish!
(define (day09-part2)
  (high-score num-players (* 100 last-marble-pts)))