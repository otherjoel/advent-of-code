#lang racket/base

(require "../aoc.rkt"
         racket/file
         racket/list
         racket/match
         racket/string)

(struct machine (lights buttons joltages) #:transparent)

(define (little-endian-lst->int lst)
  (for/fold ([z 0]) ([n (in-list lst)])
    (bitwise-ior z (arithmetic-shift 1 n))))

(define (string->lights str)
  (little-endian-lst->int
   (for/list ([(c i) (in-indexed str)] #:when (char=? #\# c)) i)))

(define (string->button str)
  (little-endian-lst->int (map string->number (string-split str ","))))

(define (parse line)
  (match-define (list l-str s1) (string-split line #rx"(?:] |\\[)"))
  (match-define (list b1 j-str) (string-split s1 #rx"(?: {|})"))
  (define b-strs (string-split b1 #rx"(?:\\(|\\) \\(|\\))"))
  (machine (string->lights l-str)
           (map string->button b-strs)
           (map string->number (string-split j-str ","))))

(define input (map parse (file->lines "day10-input.txt")))

;;------------------------------------------------

(define (press-em buttons)
  (apply bitwise-xor buttons))

(define (try-presses m)
  (match-define (machine goal buttons _) m)
  (apply min
         (for/list ([seq (in-list (combinations buttons))]
                    #:when (= (press-em seq) goal)
                    #:final (= 1 (length seq)))
           (length seq))))

(define (pt1)
  (apply + (map try-presses input)))
         
(check-answer/ns pt1 477) ; 18ms

;;------------------------------------------------

(require "day10-pt2.rkt")

(define (run-solver m)
  (solve-machine-joltage (machine-buttons m) (machine-joltages m)))

(define (pt2)
  (apply + (map run-solver input)))

(check-answer/ns pt2 17970) ; 9393ms

;;================================================

(define ex
  (string-split #<<END
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
END
                "\n"))

