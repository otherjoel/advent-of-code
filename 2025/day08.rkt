#lang racket/base

(require "../aoc.rkt"
         data/heap
         racket/file
         racket/flonum
         racket/list
         racket/match
         racket/string
         threading)

(define (parse-coords lines)
  (for/vector ([line (in-list lines)])
    (match-define (list x y z)
      (~> (string-split line ",") (map string->number _)))
    (flvector (real->double-flonum x)
              (real->double-flonum y)
              (real->double-flonum z))))

(define coords (parse-coords (file->lines "day08-input.txt")))

(define (squared-dist v1 v2)
  (define dx (fl- (flvector-ref v1 0) (flvector-ref v2 0)))
  (define dy (fl- (flvector-ref v1 1) (flvector-ref v2 1)))
  (define dz (fl- (flvector-ref v1 2) (flvector-ref v2 2)))
  (fl+ (fl* dx dx) (fl+ (fl* dy dy) (fl* dz dz))))

;;------------------------------------------------

(define (closest-n-pairs coords n)
  (define len (vector-length coords))
  
  ;; Make a heap that counts "largest" as "min"
  (define h (make-heap (λ (a b) (> (vector-ref a 2) (vector-ref b 2)))))
  
  (for* ([i (in-range len)]
         [j (in-range (add1 i) len)])
    (define d (squared-dist (vector-ref coords i) (vector-ref coords j)))
    (cond
      [(< (heap-count h) n)
       (heap-add! h (vector i j d))]
      [(< d (vector-ref (heap-min h) 2))
       (heap-remove-min! h)
       (heap-add! h (vector i j d))]))
  
  ;; Extract and reverse (heap-min gives largest of the smallest)
  (define heap-len (heap-count h))
  (define result (make-vector heap-len))
  (for ([v (in-heap/consume! h)]
        [i (in-range (sub1 heap-len) -1 -1)])
    (vector-set! result i v))
  result)

(define (make-union-find n)
  (vector (build-vector n values)  ; parent
          (make-vector n 1)))      ; size (each node starts as set of 1)

(define (uf-find uf x)
  (define parent (vector-ref uf 0))
  (define p (vector-ref parent x))
  (if (= p x)
      x
      (let ([root (uf-find uf p)])
        (vector-set! parent x root)
        root)))

(define (uf-union! uf x y)
  (define parent (vector-ref uf 0))
  (define size (vector-ref uf 1))
  (define rx (uf-find uf x))
  (define ry (uf-find uf y))
  (unless (= rx ry)
    ;; Attach smaller tree under larger (union by size)
    (define size-x (vector-ref size rx))
    (define size-y (vector-ref size ry))
    (if (< size-x size-y)
        (begin
          (vector-set! parent rx ry)
          (vector-set! size ry (+ size-x size-y)))
        (begin
          (vector-set! parent ry rx)
          (vector-set! size rx (+ size-x size-y))))))

(define (uf-all-set-sizes uf)
  ;; Returns list of all set sizes
  (define parent (vector-ref uf 0))
  (define size (vector-ref uf 1))
  (for/list ([i (in-range (vector-length parent))]
             #:when (= i (vector-ref parent i)))
    (vector-ref size i)))

(define (connected-set-sizes coords n-pairs)
  (define pairs (closest-n-pairs coords n-pairs))
  (define uf (make-union-find (vector-length coords)))
  
  (for ([p (in-vector pairs)])
    (uf-union! uf (vector-ref p 0) (vector-ref p 1)))
  
  (uf-all-set-sizes uf))

(define (pt1) (apply * (take (sort (connected-set-sizes coords 1000) >) 3)))
(check-answer/ns pt1 96672)

;;================================================

(define (find-last-connection coords)
  (define len (vector-length coords))
  (define uf (make-union-find len))
  (define num-sets len)
  
  (define pairs
    (let ([h (make-heap (λ (a b) (< (vector-ref a 2) (vector-ref b 2))))])
      (for* ([i (in-range len)]
             [j (in-range (add1 i) len)])
        (heap-add! h (vector i j (squared-dist (vector-ref coords i)
                                               (vector-ref coords j)))))
      h))

  (let loop ()
    (define p (heap-min pairs))
    (heap-remove-min! pairs)
    (define i (vector-ref p 0))
    (define j (vector-ref p 1))
    (cond
      [(= (uf-find uf i) (uf-find uf j))
       (loop)]  ; already connected, skip
      [(= num-sets 2)
       ;; This union will make it 1 = done
       (values (vector-ref coords i) (vector-ref coords j))]
      [else
       (uf-union! uf i j)
       (set! num-sets (sub1 num-sets))
       (loop)])))

(define (pt2)
  (define-values (a b) (find-last-connection coords))
  (fl* (flvector-ref a 0) (flvector-ref b 0)))

(check-answer/ns pt2 22517595.0)