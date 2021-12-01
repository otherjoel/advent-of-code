#lang racket
(require rackunit)

(define input (file->lines "day01-input.txt"))

(define start-frequency 0)

(define (day01-part1)
  (apply + start-frequency (map string->number input)))

(module+ test
  (check-equal? (day01-part1) 486)) ; Correct answer for part1

(define (day01-part2 [freq-changes (map string->number input)]
                     [start-from start-frequency]
                     [seen-frequencies (list start-frequency)])
  (define run-through
    (for/fold ([seen seen-frequencies]
               [current-freq start-from]
               #:result seen)
              ([freq-change (in-list freq-changes)])
      #:final (member (+ current-freq freq-change) seen)
      (values (append seen (list (+ current-freq freq-change))) (+ current-freq freq-change))))
  (cond
    [(check-duplicates run-through)
     (last run-through)]
    [else
     (day01-part2 freq-changes (last run-through) run-through)]))

(module+ test
  (check-equal? (day01-part2 '(3 3 4 -2 -4)) 10)
  (check-equal? (day01-part2 '(-6 3 8 5 -6)) 5)
  (check-equal? (day01-part2 '(7 7 -2 -7 -4)) 14)
  ; Actually getting the correct answer for my input takes a really long time
  ; (check-equal? (day01-part2) 69285)
  )