#lang racket

(require rackunit)

(define input 640441)

;; To try to get this code to finish running before Christmas,
;; going to try starting out with a mega-ass vector and keep
;; track of the index of the last value in a struct
(define recipes (make-vector 30000000))

(define (append-recipes! r-count val)
  (cond [(> val 9)
         (vector-set! recipes r-count 1)
         (vector-set! recipes (add1 r-count) (- val 10))
         (+ r-count 2)]
        [else
         (vector-set! recipes r-count val)
         (add1 r-count)]))

(append-recipes! 0 3)
(append-recipes! 1 7)

(struct state (rcount elf1 elf2) #:transparent)

; To start: there are 2 recipes with elves at positions 1 and 0
(define start (state 2 1 0))

(define (score current-state)
  (match-define (state recipe-count e1 e2) current-state)
  (+ (vector-ref recipes e1)
     (vector-ref recipes e2)))

(define (move-elf pos r-count)
  (modulo (+ pos (add1 (vector-ref recipes pos))) r-count))

;; For debugging
(define (list-recipes start end)
  (for/list ([i (in-range start end)])
            (vector-ref recipes i)))

;; Get the ten recipes after the given recipe
(define (get-10-after starting-state n)
  (let loop ([current-state starting-state])
    (match-define (state recipe-count e1 e2) current-state)
    (cond [(>= (- recipe-count 11) n)
           (list-recipes n (+ n 10))]
          [else
           (define new-count (append-recipes! recipe-count (score current-state)))
           (loop (state new-count (move-elf e1 new-count) (move-elf e2 new-count)))])))

(define (day14-part1 n)
  (apply string-append (map number->string (get-10-after start n))))

(module+ test
  (check-equal? (time (day14-part1 9)) "5158916779")
  (check-equal? (time (day14-part1 5)) "0124515891")
  (check-equal? (time (day14-part1 18)) "9251071085")
  (check-equal? (time (day14-part1 2018)) "5941429882")
  (check-equal? (time (day14-part1 input)) "1041411104")) ; Correct answer for part 1

(define input-part2 #(6 4 0 4 4 1))

(define (found-yet? start)
  (cond [(< start 0) #f]
        [else (for/and ([i (in-range start (+ start 6))])
                       (equal? (vector-ref input-part2 (- i start)) (vector-ref recipes i)))]))

;; How many recipes to the left of the input sequence?
(define (day14-part2)
  (let loop ([current-state start])
    (match-define (state recipe-count e1 e2) current-state)
    (define new-count (append-recipes! recipe-count (score current-state)))
    (define found?
      (cond [(equal? 2 (- new-count recipe-count))
             (or (found-yet? (- new-count 7)) (found-yet? (- new-count 6)))]
            [else (found-yet? (- new-count 6))]))
    (cond [(and found? (found-yet? (- new-count 7))) (- new-count 7)]
          [found? (- new-count 6)]
          [else
           (loop (state new-count (move-elf e1 new-count) (move-elf e2 new-count)))])))

(module+ test
  (check-equal? (time (day14-part2)) 20174745)) ; Correct answer for part 2