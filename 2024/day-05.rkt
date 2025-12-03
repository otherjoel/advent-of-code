#lang racket/base

(require "../aoc.rkt"
         racket/file
         racket/list
         racket/match
         racket/string
         threading)

(define (make-rules rule-pairs)
  (for/fold ([rules (hasheqv)])
            ([rule (in-list rule-pairs)])
    (hash-update rules (car rule) (λ (lst) (cons (cdr rule) lst)) null)))

;; "83|23\n83|62" → '#hasheqv((83 . (62 23)))
(define (lines->rules strs)
  (make-rules
   (for/list ([str (in-list strs)])
     (~> (string-split str "|")
         (map string->number _)
         (apply cons _)))))

;; "17,21,28" → '#hasheqv((17 . 0) (21 . 1) (28 . 2) (middle . 21))
(define (string->update str)
  (define nums
    (~> (string-split str ",")
        (map string->number _)))
  (hash-set 
   (for/hasheqv ([n (in-list nums)]
                 [i (in-naturals)])
     (values n i))
   'middle
   (list-ref nums (quotient (length nums) 2))))

(define (file->rules/updates filename)
  (define-values (r-strs u-strs)
    (~> (file->lines filename)
        (splitf-at non-empty-string?)))
  (values (lines->rules r-strs) (map string->update (filter non-empty-string? u-strs))))

(define (print-update? u rules)
  (for/and ([(page pos) (in-hash u)])
    (for/and ([other-page (in-list (hash-ref rules page null))])
      (< pos (hash-ref u other-page +inf.0)))))

(define (add-valid-middles updates rules)
  (for/sum ([update (in-list updates)]
            #:when (print-update? update rules))
    (hash-ref update 'middle)))

(define-values (example-rules example-updates) (file->rules/updates "day-05-example.txt"))
(define-values (input-rules input-updates) (file->rules/updates "day-05-input.txt"))

(define (part1)
  (add-valid-middles input-updates input-rules))

(check-answer/ns part1 7307) ; 1 ms

(define (swap u p1 p2)
  (define p1-new (hash-ref u p2))
  (define new-u
    (~> (hash-set u p2 (hash-ref u p1))
        (hash-set p1 p1-new)))
  (match (hash-ref new-u 'middle)
    [(== p1) (hash-set new-u 'middle p2)]
    [(== p2) (hash-set new-u 'middle p1)]
    [_ new-u]))

(define (fix-update update rules)
  (let loop ([u update])
    (define maybe-swap
      (for*/first ([p (in-list (hash-keys u))]
                   [op (in-list (hash-ref rules p null))]
                   #:when (> (hash-ref u p) (hash-ref u op +inf.0)))
        (cons p op)))
    (if maybe-swap
        (loop (swap u (car maybe-swap) (cdr maybe-swap)))
        u)))
    
(define (add-fixed-middles updates rules)
  (for/sum ([update (in-list updates)]
            #:when (not (print-update? update rules)))
    (hash-ref (fix-update update rules) 'middle)))

(define (part2)
  (add-fixed-middles input-updates input-rules))

(check-answer/ns part2 4713) ; 21 ms