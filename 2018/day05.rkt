#lang racket

(require rackunit sugar/debug)

;; For efficiency and convenience, I'll be dealing with inputs as ASCII values.
(define (ascii-list str)
  (map char->integer (string->list str)))

(define input (ascii-list (string-trim (file->string "day05-input.txt"))))

;; Treating lst as a stack, push an ASCII value on top.
;; If the new value is the same letter as the current top value
;; but differs by upper/lower case, eliminate both values.
(define (push-and-react lst val)
  (cond
    [(empty? lst) (list val)]
    [(= 32 (abs (- (car lst) val))) (rest lst)]
    [else (cons val lst)]))

;; Return a fully "reacted" version of the input list.
;; Reverses the list in the process, but the order of the finished
;; list does not matter for the solution, only the length.
(define (boil lst)
  (for/fold ([output '()])
            ([v (in-list lst)])
    (push-and-react output v)))

;; There it is: computes in 6msec!
(define (day05-part1)
  (length (boil input)))

(module+ test
  (define example-input (ascii-list "dabAcCaCBAcCcaDA"))
  (check-equal? (length (boil example-input)) 10)
  (check-equal? (day05-part1) 10584)) ; Correct answer for part one

;; Given a list and the ASCII value of an uppercase letter, first removes
;; all occurrences of that letter (both upper- and lower-case) then
;; boils the resulting list as above.
(define (remove-and-boil lst uc-asciival)
  (define (keeper? a)
    (not (member a (list uc-asciival (+ 32 uc-asciival)))))
  (boil (filter keeper? lst)))

;; For each letter of the alphabet, create a copy of input that first removes that
;; letter, then boils the list. Find the shortest such list.
;; Computes in about 550msec
(define (day05-part2)
  (define uc-letters (range 65 91))
  (define boiler (curry remove-and-boil input))
  (define boiled (map boiler uc-letters))
  (apply min (report (map length boiled))))

(module+ test
  (check-equal? (day05-part2) 6968))

;; This next has nothing to do with the solution; I was just curious which is the most
;; common letter in the original input, and if it was the removal of that letter
;; which yields the shortest boiled list. As it turns out: no.
(define (most-frequent-letter)
  (define (count-letter lst a)
    (count (lambda(c) (member c (list a (+ a 32)))) lst))
  (define letter-counts
    (map (curry count-letter input) (range 65 91)))
  (integer->char (+ 65 (index-of (report letter-counts) (apply max letter-counts)))))