#lang debug racket/base

(require "../aoc.rkt"
         racket/file
         threading)

(struct grid (rows cols vec start))

(define (grid-ref g xy)
  (vector-ref (grid-vec g) (+ (* (imag-part xy) (grid-cols g)) (real-part xy))))

(define (out? g xy)
  (not (and (< -1 (real-part xy) (grid-cols g))
            (< -1 (imag-part xy) (grid-rows g)))))

(define (lines->grid lines)
  (define rows (length lines))
  (define cols (string-length (car lines)))
  (define start #f)
  (define i -1)
  (define vec
    (for*/vector ([line (in-list lines)]
                  [ch (in-string line)])
      (set! i (add1 i))
      (when (memv ch '(#\^ #\< #\> #\v)) (set! start i))
      ch))
  (grid rows cols vec (make-rectangular (remainder start cols) (quotient start cols))))

(define-values (n e s w)
  (values 0-1i 1+0i 0+1i -1+0i))

(define dirs
  (hasheqv #\^ n #\> e  #\v s #\< w))

(define turns
  (hasheqv n e e s s w w n))

(define (count-visited g)
  (let loop ([pos (grid-start g)]
             [dir (hash-ref dirs (grid-ref g (grid-start g)))]
             [seen (hasheqv (grid-start g) #t)])
    (define new-pos (+ pos dir))
    (cond
      [(out? g new-pos) (length (hash-keys seen))]
      [(char=? #\# (grid-ref g new-pos))
       (loop pos (hash-ref turns dir) seen)]
      [else
       (loop new-pos dir (hash-set seen new-pos #t))])))

(define (follow-path g [start (grid-start g)])

(define input
  (~> (file->lines "day-06-input.txt")
      lines->grid))

(define (part1)
  (count-visited input))

(check-answer/ns part1 4977) ; 4 ms

(define seen-dirs (hasheqv n #\| s #\| e #\- w #\-))

(define (marker dir) (hash-ref seen-dirs dir))
(define (seen-dir? char dir)
  (or (char=? char #\+)
  (eqv? char (hash-ref seen-dirs dir))))

(define (count-obstruction-candidates g)
  (define start-dir (hash-ref dirs (grid-ref g (grid-start g))))
  (let loop ([pos (grid-start g)]
             [dir start-dir]
             [seen (hasheqv (grid-start g) start-dir)]
             [found null])
    (define new-pos (+ pos dir))
    (define turnt (hash-ref turns dir))
    (cond
      [(out? g new-pos) found] ; no more loop opportunities
      [(char=? #\# (grid-ref g new-pos))            ; already an obstacle there
       (loop pos turnt seen found)] 
      [else
       ; Would an obstacle at the new position put the guard on a spot
       ; they've already been, facing in the same direction as last time?
       ; Don’t check past the first obstacle
       (let follow ([i 1])
         (define pt (+ pos (* turnt i)))
         (cond
           [(or (out? g pt)
                (char=? #\# (grid-ref g pt)))
            (loop new-pos dir (hash-set seen new-pos (marker dir)) found)]
           [(eqv? turnt (hash-ref seen pt #f))
            (loop new-pos dir (hash-set seen new-pos dir) (cons new-pos found))]
           [else (follow (add1 i))]))])))

(define ex
  (lines->grid
   '("....#....."
     ".........#"
     ".........."
     "..#......."
     ".......#.."
     ".........."
     ".#..^....."
     "........#."
     "#........."
     "......#...")))