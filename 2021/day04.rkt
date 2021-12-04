#lang racket/base

(require "aoc.rkt"
         racket/file
         racket/function
         racket/port
         racket/string
         rackunit
         sugar/list)

(struct card (rows cols))

(define input (file->string "day04.txt"))

(define (read-nums!)
  (map string->number (string-split (read-line) ",")))

(define (read-cards!)
  (define a (slice-at (port->list) 5))
  (for/list ([rows (in-list (slice-at a 5))])
    (card rows (apply map list rows))))

(define (get-input [str input])
  (with-input-from-string str (lambda () (values (read-nums!) (read-cards!)))))

(define-values (numbers card-list) (get-input))

(define (mark-in-list n lst)
  (for/list ([v (in-list lst)])
    (if (eq? v n) (+ v 0+1i) v)))

(define (list-bingo? lst) (eq? 5 (apply + (map imag-part lst))))

(define (card-score c)
  (for/sum ([v (in-list (apply append (card-rows c)))])
    (if (eq? 0 (imag-part v)) v 0)))

(define (mark-on-card n c)
  (define rows (map (curry mark-in-list n) (card-rows c)))
  (define cols (map (curry mark-in-list n) (card-cols c)))
  (define marked (card rows cols))
  (define bingo?
    (and (or (ormap list-bingo? rows) (ormap list-bingo? cols)) (* n (card-score marked))))
  (or bingo? marked))

(define (part-1 [nums numbers] [cards card-list])
  (define winner? (ormap (lambda (v) (if (number? v) v #f)) cards))
  (or winner? (part-1 (cdr nums) (map (curry mark-on-card (car nums)) cards))))

(module+ test (check-answer part-1 8136)) ; part-1: 8136 (cpu: 7 real: 7 gc: 1)

(define (part-2 [nums numbers] [cards card-list])
  (define last-winner? (and (null? (cdr cards)) (number? (car cards)) (car cards)))
  (or last-winner?
      (part-2 (cdr nums)
              (map (curry mark-on-card (car nums))
                   (filter card? cards)))))

(module+ test (check-answer part-2 12738)) ; part-2: 12738 (cpu: 10 real: 11 gc: 0)

;; Testing with example inputs
(module+ test
  (define test-input
    #<<STR
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
STR
    )
  (define-values (test-nums test-cards) (get-input test-input))
  (check-equal? (part-1 test-nums test-cards) 4512)
  (check-equal? (part-2 test-nums test-cards) 1924))