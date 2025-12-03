#lang debug racket/base

(require racket/file
         racket/vector)

(struct matrix (vs sz))

(define (make-matrix strs)
  (define m (make-hasheqv))
  (define sz (string-length (car strs)))
  (for ([(line row) (in-indexed strs)])
    (for ([(c col) (in-indexed (string->list line))])
      (hash-set! m (make-rectangular col row) (- (char->integer c) 48))))
  (matrix m sz))

(define (peek m)
  (define sz (matrix-sz m))
  (for* ([y (in-range sz)] [x (in-range sz)])
    (define v (hash-ref (matrix-vs m) (make-rectangular x y)))
    (when (and (= x 0) (> y 0)) (display "\n"))
    (display (format "~a~a" (if (<= v 9) " " "") (if (= v -inf.0) "x" v)))))

(define input (make-matrix (file->lines "day11.txt")))

(define (neighbor-coords m pos)
  (define-values (px py) (values (real-part pos) (imag-part pos)))
  (for*/list ([x (in-range (sub1 px) (+ 2 px))]
              #:when (and (>= x 0) (< x (matrix-sz m)))
              [y (in-range (sub1 py) (+ 2 py))]
              #:when (and (>= y 0) (not (equal? (list px py) (list x y))) (< y (matrix-sz m))))
    (make-rectangular x y)))

(define (neighbors m pos)
  (map (λ (pos) (hash-ref (matrix-vs m) pos)) (neighbor-coords m pos)))

(define (increment-all! m)
  (for ([(pos v) (in-hash (matrix-vs m))])
    (hash-set! (matrix-vs m) pos (add1 v))))

(define (split-flashes m)
  (for/fold ([f null]
             [nonf null]
             #:result (values (list->vector f) (list->vector nonf)))
            ([(pos v) (in-hash (matrix-vs m))])
    (if (> v 9) (values (cons pos f) nonf) (values f (cons pos nonf)))))

(define (absorb-all! m)
  (define-values (flashes non-flashes) (split-flashes m))
  (define octopi (matrix-vs m))
  (cond
    [(not (vector-empty? flashes))
     (for ([pos (in-vector flashes)])
       (hash-set! octopi pos (+ v (length (filter (λ (n) (> n 9)) (neighbors m pos))))))
     (for ([pos (in-vector flashes)])
       (hash-set! octopi pos -inf.0))
     (vector-length flashes)]
    [else 0]))

(define (step! m)
  (increment-all! m)
  (define octopi (matrix-vs m))
  (let loop ([flashes (all-flash-spots m)]
             [total-flashes 0])
    (cond
      [(not (vector-empty? flashes))
       
       (for ([pos (in-vector flashes)])
         (hash-set! octopi pos -inf.0))
       (loop (all-flash-spots m) (+ total-flashes (vector-length flashes)))]
      [else
       (for ([(pos v) (in-hash octopi)] #:when (= v -inf.0))
         (hash-set! octopi pos 0))
       total-flashes])))

(define t1
  (make-matrix '("11111"
                 "19991"
                 "19191"
                 "19991"
                 "11111")))

(define t2
  (make-matrix
   '("5483143223"
     "2745854711"
     "5264556173"
     "6141336146"
     "6357385478"
     "4167524645"
     "2176841721"
     "6882881134"
     "4846848554"
     "5283751526")))

