#lang racket/base

(require "../aoc.rkt"
         racket/file
         racket/match
         racket/string
         threading)

;; manifold
(struct m (start width rows) #:transparent)

(define (parse lines)
  (m (string-find (car lines) "S")
     (string-length (car lines))
     (for/fold ([acc '()] #:result (reverse acc))
               ([l (in-list lines)])
       (cons (for/list ([(c pos) (in-indexed l)]
                        #:when (eq? c #\^)) pos) acc))))

;; Part 1 ----------------------------------------
(struct result (splits beams) #:transparent)

(define (add-at s posn v w) (if (< -1 posn w) (hash-update s posn (λ (n) (+ v n)) 0) s))
(define (beam-at? s posn) (hash-ref s posn #f))

(define (split-at s posn w)
  (define beam-val (hash-ref s posn))
  (~> (add-at s (sub1 posn) beam-val w)
      (add-at (add1 posn) beam-val w)
      (hash-remove posn)))

(define (shoot manifold)
  (match-define (m start width rows) manifold)
  (for*/fold ([splits 0]
              [beams (hasheq start 1)]
              #:result (result splits beams))
             ([row (in-list rows)]
              [splitter (in-list row)]
              #:when (beam-at? beams splitter))
    (values (add1 splits) (split-at beams splitter width))))

(define input (parse (file->lines "day07-input.txt")))

(define (pt1) (result-splits (shoot input)))
(check-answer/ns pt1 1543 10) ; 436 μs

;; Part 2 ----------------------------------------
(define (count-timelines s)
  (for/sum ([(k v) (in-hash s)]) v)) ; seems faster than apply +

(define (pt2) (count-timelines (result-beams (shoot input))))
(check-answer/ns pt2 3223365367809 10) ; 329 μs
