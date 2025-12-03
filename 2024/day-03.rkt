#lang racket/base

(require "../aoc.rkt"
         racket/file
         racket/match
         racket/string)

(define input (file->string "day-03-input.txt"))

(define mul-rx #rx"mul\\(([0-9]+),([0-9]+)\\)")

(define (find-muls str)
  (regexp-match* mul-rx str #:match-select cdr))

(define (part1)
  (for/sum ([pr (in-list (find-muls input))])
    (apply * (map string->number pr))))

(check-answer/ns part1 159833790) ; part1: 159833790 (829 μs)

(define mul-do/dont-rx #rx"mul\\(([0-9]+),([0-9]+)\\)|don\\'t\\(\\)|do\\(\\)")

(define (do/dont-muls str)
  (for/fold ([valid-muls '()]
             [last-do/dont 'do]
             #:result (apply + valid-muls))
            ([instr (in-list (regexp-match* mul-do/dont-rx str #:match-select values))])
    (match instr
      [(list* "don't()" _rest) (values valid-muls 'dont)]
      [(list* "do()" _rest) (values valid-muls 'do)]
      [(list _mul x y)
       (if (eq? last-do/dont 'do)
           (values (cons (apply * (map string->number (list x y))) valid-muls) last-do/dont)
           (values valid-muls last-do/dont))])))

(define (part2) (do/dont-muls input))

(check-answer/ns part2 89349241) ; part2: 89349241 (842 μs)