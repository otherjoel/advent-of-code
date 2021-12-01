#lang racket

(require sugar rackunit)

(define input
  (sort (file->lines "day04-input.txt") string<?))

;; Pull apart the strings
(define (parse-line line)
  (define pattern #px"\\[\\d{4}-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})\\] (?:Guard #|falls |)(\\d+|asleep|wakes)")
  (define parsed (regexp-match pattern line))
  (append (map string->number (sublist parsed 1 5)) (list (last parsed))))

;; Local Convenience
(define (hash-with-key h key)
  (hash-set h key (or (hash-ref h key #f) '())))

(define (add-to-end lst v)
  (reverse (cons v (reverse lst))))

;; Returns a hash table of guards => list of wake/sleep logs in chrono order
(define (lines->guards/hash loglines)
  (for/fold ([guards (hash)]
             [current-guard ""]
             #:result guards)
            ([l (in-list (map parse-line loglines))])
    (cond
      [(member (last l) '("asleep" "wakes"))
       (values (hash-set guards current-guard (add-to-end (hash-ref guards current-guard) l)) current-guard)]
      [else (values (hash-with-key guards (last l))
                    (last l))])))

;; Consolidate a list whose entries are alternating sleep/wake times into a
;; list of sleep/wake minute pairs
(define (sleeptimes->pairs sleeptimes)
  (for/list ([twolines (in-list (slice-at sleeptimes 2))])
            (match twolines
              [(list (list _ _ _ sleepmin _)
                     (list _ _ _ wakemin _))
               (list sleepmin wakemin)])))

;; NOW: Use all the above to build a hash table where key:value is guard:list of all minute-pairs
(define guard-sleeptime-pairs
  (let ([guardlog (lines->guards/hash input)])
    (for/hash ([(guard lines) (in-hash guardlog)])
              (values guard (sleeptimes->pairs lines)))))

;; Converts '(a b) into a 60-position vector where values at indexes a through (b-1) are 1
;; (each position a minute of the hour; the 1s indicate minutes where the guard was asleep)
(define (pair->minutemap p)
  (vector-append (make-vector (first p))
                 (make-vector (- (second p) (first p)) 1)
                 (make-vector (- 60 (second p)))))

;; Use vector addition to sum up all the sleep-minute-maps for each guard,
;; showing when and how often they were asleep during their shifts
(define guard-sleepmaps
  (for/list ([guard (in-hash-keys guard-sleeptime-pairs)])
            (cons guard
                  (for/fold ([smap (make-vector 60)])
                            ([minutemap (in-list (hash-ref guard-sleeptime-pairs guard))])
                    (vector-map + (pair->minutemap minutemap) smap)))))

;; For each guard: '(guard-ID [asleep min count] [max sleeps during any minute] [minute with max sleeps])
(define analysis
  (for/list ([g (in-list guard-sleepmaps)])
            (let* ([minutes (vector->list (cdr g))]
                   [max-sleeps (apply max minutes)])
              (list (car g)
                    (vector-count positive? (cdr g))
                    max-sleeps
                    (index-of minutes max-sleeps)))))

;; Comparison function for sorting analysis
(define (guard-slept-more-mins? a b)
  (or (> (second a) (second b))    ; was asleep more minutes out of the hour
      (and (= (second a) (second b))  ; OR was asleep the same number of minutes…
           (> (third a) (third b)))))     ;  …but had more sleeps in a given minute

;; Computers answers of the form given in the puzzle:
;; "What is the ID of the guard you chose multiplied by the minute you chose?"
(define (compute-using lst)
  (let* ([winner-stats (first lst)]
         [guard-id (string->number (first winner-stats))]
         [minute (last winner-stats)])
    (* guard-id minute)))

;; Strategy 1: Find the guard with the most minutes asleep and the minute
;; that guard spends asleep the most.
(define (day04-part1)
  (compute-using (sort analysis guard-slept-more-mins?)))

(module+ test
  (check-equal? (day04-part1) 99759))  ; Correct answer for part 1

;; Strategy 2: Find the guard that is most frequently asleep on the same minute.
(define (day04-part2)
  (compute-using (sort analysis > #:key third)))

(module+ test
  (check-equal? (day04-part2) 97884))