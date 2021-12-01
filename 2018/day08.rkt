#lang racket

(require rackunit)

(define input (file->list "day08-input.txt"))

;; Returns (values metadata lst-remainder)
(define (sum-of-metadata lst [metas '()])
  (match-define (list children metacount) (take lst 2))
  (define remainder (drop lst 2))
  (cond
    [(= 0 children) (values (append metas (take remainder metacount)) (drop remainder metacount))]
    [else
     (define-values (new-metas new-rest)
       (for/fold ([metas-accum metas]
                  [rest-lst remainder])
                 ([c (in-range children)])
         (sum-of-metadata rest-lst metas-accum)))
     (values (append new-metas (take new-rest metacount)) (drop new-rest metacount))]))

(define (day08-part1 lst)
  (define-values (metas _) (sum-of-metadata lst))
  (apply + metas))

(module+ test
  (define example '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))
  
  (check-equal? (day08-part1 example) 138)        ; Matches example result on AoC website
  (check-equal? (time (day08-part1 input)) 40908) ; Correct answer for part 1

  ; Example input from reddit that helped me finish
  ; Illustrates problem of zero-element children
  (define reddit-example '(2 2 1 1 0 0 10 0 2 20 30 40 50))
  (check-equal? (day08-part1 reddit-example) 150))

;; Per Part 2, when a node has children its metadata are read as references to the values of those
;; children nodes, and its value is the some of the values of the children referred to.
;; This function applies the problem-specified rules for reading and scoring metadata with child nodes:
(define (ref-and-sum child-vals refs)
  (define (ref n)
    (cond [(zero? n) 0]                           ; “A metadata entry of 0 refers to nothing”
          [(> n (length child-vals)) 0]           ; “If a referenced child node does not exist, skip it.”
          [else (list-ref child-vals (sub1 n))])) 
  (apply + (map ref refs)))                       ; “A child node can be referenced multiple times”

;; Returns (values node-value remaining-lst)
(define (node-value lst)
  (match-define (list children metacount) (take lst 2))
  (define remainder (drop lst 2))

  (cond
    [(= children 0) (values (apply + (take remainder metacount)) (drop remainder metacount))]
    [else
     (define-values (child-values new-rest)
       (for/fold ([vals-accum '()]
                  [rest-lst remainder])
                 ([c (in-range children)])
         (let-values ([(child-val next-rest) (node-value rest-lst)])
           (values (append vals-accum (list child-val)) next-rest))))
     (values (ref-and-sum child-values (take new-rest metacount)) (drop new-rest metacount))]))

(define (day08-part2)
  (let-values ([(answer _) (node-value input)])
    answer))

(module+ test
  (check-equal? (time (day08-part2)) 25910)) ; Correct answer for part 2