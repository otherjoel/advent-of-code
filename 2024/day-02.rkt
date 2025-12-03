#lang racket/base

(require "../aoc.rkt"
         racket/file
         racket/string)

(define (load-input)
  (for/list ([line (in-list (file->lines "day-02-input.txt"))])
    (map string->number (string-split line))))

(define (is-safe? lst [dampen-proc #f])
  (or
   (and
    (or (apply > lst)
        (apply < lst))
    (for/fold ([last (car lst)]
               [remain (cdr lst)]
               #:result (null? remain))
              ([i (in-list (cdr lst))])
      #:break (> (abs (- last i)) 3)
      (values (car remain) (cdr remain))))
   (and
    dampen-proc
    (dampen-proc lst))))

(define input (load-input))

(define (part1)
  (length (filter is-safe? input)))

(check-answer/ns part1 559) ;; part1: 559 (92 μs)

(define (dampener lst)
  (let loop ([front '()]
             [back (cdr lst)]
             [i (car lst)])
    (cond
      [(is-safe? (append front back)) #t]
      [(null? back) #f]
      [else (loop (append front (list i))
                  (cdr back)
                  (car back))])))

(define (part2)
  (length (filter (λ (lst) (is-safe? lst dampener)) input)))

(check-answer/ns part2 601) ;; part2: 601 (339 μs)

(define example-input
  '((7 6 4 2 1)
    (1 2 7 8 9)
    (9 7 6 2 1)
    (1 3 2 4 5)
    (8 6 4 4 1)
    (1 3 6 7 9)))