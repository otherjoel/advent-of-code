#lang racket/base

(require racket/file
         racket/vector)

(define delims #hasheqv((#\< . #\>) (#\( . #\)) (#\[ . #\]) (#\{ . #\})))
(define openers (hash-keys delims))

(define (consume tokens [stack null])
  (cond
    [(null? tokens) stack]
    [(member (car tokens) openers) (consume (cdr tokens) (cons (car tokens) stack))]
    [(eqv? (car tokens) (hash-ref delims (car stack)))
     (consume (cdr tokens) (cdr stack))]
    [else (car tokens)]))

(define input (map string->list (file->lines "day10.txt")))

(define (part-1 [lst input])
  (apply + (map (λ (c) (hash-ref #hasheqv((#\) . 3) (#\] . 57) (#\} . 1197) (#\> . 25137)) c 0))
                (map consume lst))))

(define closer-scores (hasheqv #\( 1 #\[ 2 #\{ 3 #\< 4))

(define (autocomplete-score lst [current 0])
  (for/fold ([score 0])
            ([token (in-list lst)])
    (+ (hash-ref closer-scores token) (* 5 score))))

(define (part-2)
  (define sorted
    (vector-sort
     (for/vector ([lst (in-list (map consume input))]
                  #:when (list? lst))
       (autocomplete-score lst))
     <))
  (vector-ref sorted (floor (/ (vector-length sorted) 2))))

(module+ test
  (require "aoc.rkt")
  (check-answer/ns part-1 394647)     ; part-1: 394647 (435 μs)
  (check-answer/ns part-2 2380061249) ; part-2: 2380061249 (446 μs)
  
  (define (consume-str str) (consume (string->list str)))
  (check-equal? (consume-str "([])") '())
  (check-equal? (consume-str "{()()()}") '())
  (check-equal? (consume-str "<([{}])>") '())
  (check-equal? (consume-str "[<>({}){}[([])<>]]") '())
  (check-equal? (consume-str "(((((((((())))))))))") '())
  
  (check-equal? (consume-str "{([(<{}[<>[]}>{[]{[(<()>") #\})
  (check-equal? (consume-str "[[<[([]))<([[{}[[()]]]") #\))
  (check-equal? (consume-str "[{[{({}]{}}([{[{{{}}([]") #\])
  (check-equal? (consume-str "[<(<(<(<{}))><([]([]()") #\))
  (check-equal? (consume-str "<{([([[(<>()){}]>(<<{{") #\>)

  (check-equal? (autocomplete-score (consume-str "[({(<(())[]>[[{[]{<()<>>")) 288957)
  (check-equal? (autocomplete-score (consume-str "[(()[<>])]({[<{<<[]>>(")) 5566)
  (check-equal? (autocomplete-score (consume-str "(((({<>}<{<{<>}{[]{[]{}")) 1480781)
  (check-equal? (autocomplete-score (consume-str "{<[[]]>}<{[{[{[]{()[[[]")) 995444)
  (check-equal? (autocomplete-score (consume-str "<{([{{}}[<[[[<>{}]]]>[]]")) 294))