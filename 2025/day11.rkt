#lang racket/base

(require "../aoc.rkt"
         racket/file
         racket/match)

(define (table lines)
  (for*/hash ([line (in-list lines)])
    (match-define (list* device outs)
      (map string->symbol (regexp-match* #px"([a-z]{3})" line)))
    (values device outs)))

(define input (table (file->lines "day11-input.txt")))

;;------------------------------------------------

(define (viable-paths table [start 'you])
  (for/sum ([path (in-list (hash-ref table start))])
    (if (eq? 'out path) 1 (viable-paths table path))))

(define (pt1) (viable-paths input))

(check-answer/ns pt1 534 3) ; 39 μs

;;------------------------------------------------
(define (dac/fft t)
  (+ (if (hash-ref t 'dac #f) 1 0)
     (if (hash-ref t 'fft #f) 2 0)))

(define (node/d/f node d/f)
  (string->symbol (format "~a~a" node d/f)))

(define viable/dac+fft
  (let ([nodes (make-hasheq)])
    (λ (table [start 'svr] [seen (hasheq)])
      (for/sum ([path (in-list (hash-ref table start))])
        (define d/f (dac/fft seen))
        (cond
          [(eq? 'out path)
           (if (= 3 d/f) 1 0)]
          [else
           (hash-ref!
            nodes
            (node/d/f path d/f)
            (λ () (viable/dac+fft table path (hash-set seen path 'y))))])))))

(define (pt2)
  (viable/dac+fft input))

(check-answer/ns pt2 499645520864100 3) ; 440 μs
