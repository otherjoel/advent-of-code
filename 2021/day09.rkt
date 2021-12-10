#lang racket/base

(require "aoc.rkt"
         racket/file
         racket/match
         sugar/cache)

(define rect make-rectangular)

(define (parse! [p (current-input-port)])
  (define m (make-hasheqv))
  (let loop ([row 0] [col 0])
    (match (read-byte p)
      [(? eof-object?) m]
      [10 (loop (add1 row) 0)]
      [(var b) (hash-set! m (rect col row) (- b 48)) (loop row (+ 1 col))])))

(define/caching (topo-bounds t)
  (list (apply max (map real-part (hash-keys t)))
        (apply max (map imag-part (hash-keys t)))))

(define input (with-input-from-file "day09.txt" parse!))

(define (neighbors t pos)
  (match-define (list ux uy) (topo-bounds t))
  (define x (real-part pos))
  (define y (imag-part pos))
  (filter (λ (x) x) 
          (list (and (> x 0) (rect (sub1 x) y))
                (and (< x ux)  (rect (add1 x) y))
                (and (> y 0) (rect x (sub1 y)))
                (and (< y uy) (rect x (add1 y))))))

(define (neighbor-vals topo pos)
  (map (λ (x) (hash-ref topo x)) (neighbors topo pos)))

(define (low-spot-risklevels topo)
  (for/sum ([(pos v) (in-hash topo)]
            #:when (andmap (λ (n) (< v n)) (neighbor-vals topo pos)))
    (add1 v)))

(define (part-1) (low-spot-risklevels input))

(module+ test (check-answer part-1 560)) ; part-1: 560 (cpu: 4226 real: 4338 gc: 5) 😜

(define (downhill-step topo pos)
  (define v (hash-ref topo pos))
  (findf (λ (n) (< (hash-ref topo n) v))
         (neighbors topo pos)))

(define/caching (trace-low-spot topo pos)
  (let ([next (downhill-step topo pos)])
    (cond [(not next) pos]
          [else (trace-low-spot topo next)])))

(define (find-basin-sizes topo)
  (define basins (make-hasheqv))
  (for ([(pos v) (in-hash topo)] #:when (< v 9))
    (let ([end (trace-low-spot topo pos)])
      (hash-set! basins end (cons pos (hash-ref basins end '())))))
  (sort (map length (hash-values basins)) >))

(define (largest-basins-product topo)
  (match (find-basin-sizes topo) [(list b1 b2 b3 bs ...) (* b1 b2 b3)]))

(define (part-2)
  (largest-basins-product input))

(module+ test (check-answer part-2 959136)) ; part-2: 959136 (cpu: 15548 real: 15966 gc: 13) 🤮

(module+ test
  (define test-input #<<EOF
2199943210
3987894921
9856789892
8767896789
9899965678
EOF
    )
  (define test-topo (parse! (open-input-string test-input)))
  (check-equal? (low-spot-risklevels test-topo) 15)
  (check-equal? (largest-basins-product test-topo) 1134))