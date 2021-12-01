#lang racket

(require rackunit)

;; Reading and parsing the input
;; We want the initial state as a string and the rules in a hash table.
(define (read-input filename)
  (match-define (list init-line _ rule-lines ...) (file->lines filename))
  (define pots (substring init-line 15 (string-length init-line)))
  (define rules
    (for/hash ([str (in-list rule-lines)])
              (apply values (rest (regexp-match #px"([\\.|#]{5}) => (\\.|#)" str)))))
  (values pots rules))

(define-values (initial-state rules) (read-input "day12-input.txt"))

;; Just returns a string of length n filled with "."
(define (make-pots n)
  (list->string (make-list n #\.)))

;; Returns a string 5 characters in length showing the given pot in the middle with the two
;; pots to either side. Fills in empty pots as needed on either end.
(define (get-pot-sitch state index)
  (define-values (extended-state ext-len)
    (cond [(< 1 index (- (string-length state) 2)) (values state 0)]
          [(< index 2) (values (string-append (make-pots (+ 2 (abs index))) state) (+ 2 (abs index)))]
          [else (define pots-needed (- index (- (string-length state) 3)))
                (values (string-append state (make-pots pots-needed)) 0)]))
  (substring extended-state (+ index ext-len -2) (+ index ext-len 3)))

(module+ test
  (define test ".#.##..###")
  (check-equal? (get-pot-sitch test 0)  "...#.")
  (check-equal? (get-pot-sitch test -1) "....#")
  (check-equal? (get-pot-sitch test -2) ".....")
  (check-equal? (get-pot-sitch test 7)  "..###")
  (check-equal? (get-pot-sitch test 9)  "###..")
  (check-equal? (get-pot-sitch test 11) "#...."))

;; Returns the state of the pot in the next generation
;; Returns "." in the event of a missing rule (allows use of the example inputs)
(define (next-pot-state state index)
  (hash-ref rules (get-pot-sitch state index) "."))

(define (evolve-pots-range state [start 0] [end (string-length state)])
  (for/list ([i (in-range start end)]) (next-pot-state state i)))

;; Calculates the state of the pots after specified generations
;; Returns both the string representation and the value of the left-most pot
(define (evolve-pots generation-limit)
  (for/fold ([pots initial-state]
             [current-start 0])
            ([gen (in-range generation-limit)])
    (let* ([left-end (evolve-pots-range pots -2 0)]
           [left-end (or (member "#" left-end) '())]
           [new-start (- current-start (length left-end))]
           [right-end (evolve-pots-range pots (string-length pots) (+ 2 (string-length pots)))]
           [right-end (reverse (or (member "#" (reverse right-end)) '()))])
      (values (apply string-append (append left-end (evolve-pots-range pots) right-end)) new-start))))

;; Returns the sum of values of all the pots that contain plants
(define (pot-values pots first-val)
  (define plist (string->list pots))
  (apply +
         (for/list ([i (in-range 0 (length plist))]
                    [v (in-range first-val (- (length plist) first-val))])
                   (if (equal? #\# (list-ref plist i)) v 0))))

;; How many potted plants are there after 20 generations?
(define (day12-part1)
  (define-values (pots first-pot) (evolve-pots 20))
  (pot-values pots first-pot))

(module+ test
  (check-equal? (day12-part1) 3605)) ; Correct answer for part 1

;; How many potted plants exist after 50,000,000,000 generations?
;; Manual inspection of the output shows that after the 98th generation,
;; every further generation increases the number of potted plants by 81.
(define (day12-part2)
  (define-values (pots first-pot) (evolve-pots 98))
  (let ([at-98 (pot-values pots first-pot)])
    (+ at-98 (* (- 50000000000 98) 81))))

(module+ test
  (check-equal? (day12-part2) 4050000000798)) ; Correct answer for part 2