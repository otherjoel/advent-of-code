#lang racket/base

(require "aoc.rkt"
         racket/list
         racket/match
         racket/set
         racket/vector)

(define (parse! [p (current-input-port)])
  (define m (make-hasheqv))
  (let loop ([row 0] [col 0])
    (match (read-byte p)
      [(? eof-object?) m]
      [10 (loop (add1 row) 0)]
      [(var b)
       (hash-set! m (make-rectangular col row) (- b 48))
       (loop row (+ 1 col))])))

(define input (with-input-from-file "day09.txt" parse!))

(define (mapref t x y) (hash-ref t (make-rectangular x y) 10))

(define (low-spot-risklevels topo)
  (for/sum ([(pos v) (in-hash topo)])
    (define x (real-part pos))
    (define y (imag-part pos))
    (or (and (< v (mapref topo (sub1 x) y))
             (< v (mapref topo (add1 x) y))
             (< v (mapref topo x (sub1 y)))
             (< v (mapref topo x (add1 y)))
             (add1 v))
        0)))

(define (part-1) (low-spot-risklevels input))

(module+ test (check-answer/ns part-1 560)) ; part-1: 60 (9796 μs)

(define (neighbors pos)
  (let ([x (real-part pos)]
        [y (imag-part pos)])
    (list
     (make-rectangular (sub1 x) y)
     (make-rectangular (add1 x) y)
     (make-rectangular x (sub1 y))
     (make-rectangular x (add1 y)))))

(define (basin-size topo pos)
  (define visited (mutable-seteqv))
  (let loop ([p pos])
    (define v (hash-ref topo p 9))
    (cond
      [(= v 9) 0]
      [else
       (add1 (for*/sum ([n (in-list (neighbors p))]
                        #:unless (set-member? visited n)
                        #:when (< v (hash-ref topo n -1)))
               (set-add! visited n)
               (loop n)))])))

(define (measure-basins topo)
  (define szs
    (for/vector ([(pos v) (in-hash topo)]
               #:when (andmap (λ (np) (< v (hash-ref topo np 10)))
                              (neighbors pos)))
      (basin-size topo pos)))
  (for/product ([v (in-vector (vector-take (vector-sort szs >) 3))]) v))

(define (part-2) (measure-basins input))

(module+ test (check-answer/ns part-2 959136) ; part-2: 959136 (26472 μs)

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
  (check-equal? (measure-basins test-topo) 1134))