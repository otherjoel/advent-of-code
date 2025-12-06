#lang racket/base

(require racket/file
         racket/list
         racket/match
         "../aoc.rkt")

(define (parse/1 lines)
  (match-define (cons ops-line num-lines) (reverse lines))
  (for/sum ([p (in-list (regexp-match-positions* #rx"[*+][ ]+" ops-line))])
    (match-define (cons start end) p)
    (define nums
      (for/list ([nl (in-list num-lines)])
        (for/fold ([acc 0]) ([c (in-string nl start end)])
          (if (eq? #\space c) acc (+ (* acc 10) (- (char->integer c) 48))))))
    (apply (if (eq? (string-ref ops-line start) #\*) * +) nums)))

(define input (file->lines "day06-input.txt"))

(define (pt1) (parse/1 input))
(check-answer/ns pt1 7098065460541) ; 466 μs

;; Part 2 ----------------------------------------

(define (digit-ref line pos)
  (define cv (char->integer (string-ref line pos)))
  (and (not (= 32 cv)) (- cv 48)))

(define (parse/2 lines)
  (match-define (cons ops-line num-lines) (reverse lines))
  (for/sum ([p (in-list (regexp-match-positions* #rx"[*+][ ]+" ops-line))])
    (match-define (cons start end) p)
    (define transposed-nums
      (for*/list ([col (in-range start end)]
                  [digits (in-value (filter-map (λ (l) (digit-ref l col)) num-lines))]
                  #:unless (null? digits))
        (foldl (λ (d acc) (+ d (* acc 10))) 0 (reverse digits))))
    (apply (if (eq? (string-ref ops-line start) #\*) * +) transposed-nums)))

(define (pt2)
  (parse/2 input))

(check-answer/ns pt2 13807151830618) ; 596 μs


