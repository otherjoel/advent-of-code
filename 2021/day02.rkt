#lang racket/base

(require "aoc.rkt"
         racket/file
         rackunit)

(define dirs (hasheq 'forward 1 'up 0-1i 'down 0+1i))

(define (read-direction in-port)  
  (define next (read in-port))
  (if (eof-object? next)
      next
      (* (hash-ref dirs next) (read in-port))))

(define input (file->list "day02.txt" read-direction))

(define (part-1)
  (define sum (apply + input))
  (* (real-part sum)
     (imag-part sum)))

(module+ test
  (check-answer part-1 1882980)) ; part-1: 1882980 (cpu: 0 real: 0 gc: 0)

(define (part-2)
  (for/fold ([pos 0]
             [aim 0]
             #:result (* (real-part pos) (imag-part pos)))
            ([move (in-list input)])
    (values (+ pos (make-rectangular (* 1 (real-part move)) (* aim (real-part move))))
            (+ aim (imag-part move)))))

(module+ test
  (check-answer part-2 1971232560)) ; part-2: 1971232560 (cpu: 0 real: 0 gc: 0)