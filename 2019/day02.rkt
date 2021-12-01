#lang racket/base

(require threading
         racket/vector
         racket/list
         racket/match
         racket/string
         racket/file)

(define input
  (~> (file->string "day02-input.txt")
      (string-split ",")
      (map string->number _)
      list->vector))

; Get a sub-vector out of a vector; if len is too high, return as
; many elements as possible
(define (vector-slice vec start len)
  (define coerce-end (min (+ start len) (vector-length vec)))
  (vector-copy vec start coerce-end))

; vector-ref, but return the last element when the index is too high
(define (coerce-ref vec i)
  (vector-ref vec (min i (sub1 (vector-length vec)))))

(define opcodes
  (hash 1 + 
        2 *
        99 (lambda _ #f)))

(define (do-op! vec start)
  (match (vector-slice vec start 4)
    [(vector opcode loc1 loc2 loc3)
     (define func (hash-ref opcodes opcode (lambda _ (lambda _ #f))))
     (and~>> (func (coerce-ref vec loc1)
                   (coerce-ref vec loc2))
             (vector-set! vec loc3))]
    [else #f])) ; if the slice has less than 4 elements it's the end

(define (run-prog! vec [noun #f] [verb #f] #:result-idx [result 0])
  (when noun (vector-set! vec 1 noun))
  (when verb (vector-set! vec 2 verb))
  (let loop ([cursor 0])
    (cond [(do-op! vec cursor)
           (loop (+ cursor 4))]
          [else (vector-ref vec result)])))

; Test cases given by the problem
(module+ test
  (require rackunit)

  (check-equal? (run-prog! (vector 1 9 10 3 2 3 11 0 99 30 40 50)) 3500)

  (define test2 (vector 1 0 0 0 99))
  (define test3 (vector 2 3 0 3 99))
  (define test4 (vector 2 4 4 5 99 0))
  (define test5 (vector 1 1 1 4 99 5 6 0 99))
  
  (check-equal? (run-prog! test2) 2)
  (check-equal? (run-prog! test3 #:result-idx 3) 6)
  (check-equal? (run-prog! test4 #:result-idx 5) 9801)
  (check-equal? (run-prog! test5) 30))

; Important that we run on a copy of the original input to avoid mutating it
; before the second part.
(define (day02-part1)
  (run-prog! (vector-copy input) 12 2))

(module+ test
  ; cpu time: 0 real time: 0 gc time: 0
  (check-equal? (time (day02-part1)) 5866663)) ; part 1 answer for my input

(define (day02-part2)
  (define desired-out 19690720)

  (for*/or ([noun (in-range 100)]
            [verb (in-range 100)])
    (cond [(equal? (run-prog! (vector-copy input) noun verb)
                   desired-out)
           (+ (* 100 noun) verb)]
          [else #f])))

(module+ test
  ; cpu time: 78 real time: 77 gc time: 46
  (check-equal? (time (day02-part2)) 4259))  ; part 2 answer for my input