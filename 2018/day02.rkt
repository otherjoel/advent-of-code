#lang racket

(require rackunit)

(define input (file->lines "day02-input.txt"))

;; Returns hash table where keys are unique chars in str,
;; values are # of occurrences of that char in str
(define (check-id str)
  (for/fold ([seen (hash)])
            ([c (in-list (string->list str))])
    (hash-set seen c (add1 (hash-ref seen c 0)))))

;; Returns hash table where keys are numbers, values are counts of
;; how many ids in id-list that have a character that repeats exactly
;; [key] times
(define (id-counts id-list)
  (for/fold ([counts (hash)])
            ([id (in-list id-list)])
    (for/fold ([each-count counts])
              ([c (in-list (remove-duplicates (hash-values (check-id id))))])
      (hash-set each-count c (add1 (hash-ref each-count c 0))))))

(define (day01-part1 id-list)
  (define counts (id-counts id-list))
  (* (hash-ref counts 2 0) (hash-ref counts 3 0)))

(module+ test
  (define test-ids '("abcdef"
                     "bababc"
                     "abbcde"
                     "abcccd"
                     "aabcdd"
                     "abcdee"
                     "ababab"))
  (check-equal? (day01-part1 test-ids) 12)
  (check-equal? (day01-part1 input) 7533)) ; Correct answer for part 1

(define part2-example
  '("abcde"
    "fghij"
    "klmno"
    "pqrst"
    "fguij"
    "axcye"
    "wvxyz"))

;; Returns a list of characters at positions where id1 and id2 match
(define (compare-box-ids id1 id2)
  (define id1-chars (string->list id1))
  (define id2-chars (string->list id2))
  (filter identity (for/list ([i (in-range (length id1-chars))])
                     (if (equal? (list-ref id1-chars i) (list-ref id2-chars i))
                         (list-ref id1-chars i)
                         #f))))

(define (day02-part2 ids)
  (define (is-match? comparison-chars)
    (equal? (length comparison-chars) (sub1 (string-length (first ids)))))
  (define matches
    (filter is-match? (map (curry compare-box-ids (first ids)) (rest ids))))
  (if (not (empty? matches)) (list->string (first matches)) (day02-part2 (rest ids))))
  
(module+ test
  (check-equal? (day02-part2 part2-example) "fgij")
  (check-equal? (day02-part2 input) "mphcuasvrnjzzkbgdtqeoylva")) ; Correct answer for part 2