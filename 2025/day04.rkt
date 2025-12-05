#lang racket/base

(require racket/file
         racket/list
         racket/string
         "../aoc.rkt")

(define max-neighbors 4)

(define (parse-grid rows)
  (define g (make-hash))
  (for* ([(line row) (in-indexed rows)]
         [(c col) (in-indexed line)]
         #:when (eqv? c #\@))
    (hash-set! g (make-rectangular col row) 1))
  g)

(define input (file->lines "day04-input.txt"))
(define grid (parse-grid input))
(define rows (length input))
(define cols (string-length (car input)))

;; Part 1 -----------------------------------------

(define (grid-ref coord)
  (hash-ref grid coord 0))

(define (in-bounds? coord)
  (and (< -1 (real-part coord) cols)
       (< -1 (imag-part coord) rows)
       coord))
  
(define (neighbor-coords coord)
  (filter-map
   in-bounds?
   (for*/list ([n (in-list '(-1-1i -1 -1+1i 0-1i 0+1i 1-1i 1 1+1i))])
     (+ n coord))))

(define (count-neighbor-rolls coord)
  (for/sum ([coord (in-list (neighbor-coords coord))])
    (grid-ref coord)))

(define (accessible-rolls)
  (for/list ([coord (in-hash-keys grid)]
             #:when (< (count-neighbor-rolls coord) max-neighbors))
    coord))

(define (pt1) (length (accessible-rolls)))
(check-answer/ns pt1 1551)

;; Part 2 -----------------------------------------

(define (remove-rolls)
  (define rolls-to-remove (accessible-rolls))
  (cond
    [(null? rolls-to-remove) 0]
    [else
     (for ([roll (in-list rolls-to-remove)]) (hash-remove! grid roll))
     (+ (length rolls-to-remove) (remove-rolls))]))

(check-answer/ns remove-rolls 9784)


