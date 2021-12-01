#lang racket/base

(require rackunit
         racket/list
         racket/match
         racket/function)

;; 123446 ==> '(1 2 3 4 4 6) using a lot of dividing and multiplying
;; no idea if this is faster or not
(define (to-list num)
  (for/list ([p (in-range 5 -1 -1)])
    (define a (quotient num (expt 10 p)))
    (- a (* 10 (quotient a 10)))))

(define (p1-good? lst)
  (and (apply <= lst)
       (match lst
         [(list _ ... a a _ ...) #t]
         [_ #f])))

(define start 168630)
(define end 718098)

(define (day4-p1)
  (define nums (range start (+ 1 end)))
  (define pwds (map to-list nums))
  (length (filter p1-good? pwds)))

(check-equal? (time (day4-p1)) 1686) ;; correct part 1 answer for my input

;; '(1 2 3 4 4 6) ==> '((1) (2) (3) (4 4) (6))
(define (grps lst)
  (define working (reverse lst))
  (let loop ([so-far `((,(car working)))] [remaining (cdr working)])
    (define last-grp (first so-far))
    (cond
      [(empty? remaining) so-far]
      [(equal? (first remaining) (first last-grp))
       (loop (cons (cons (first remaining) last-grp) (rest so-far))
             (rest remaining))]
      [else
       (loop (cons (list (first remaining)) so-far) (rest remaining))])))

(define (p2-good? lst)
  (and (apply <= lst)
       (ormap (curry equal? 2) (map length (grps lst)))))

(define (day4-p2)
  (define nums (range start (+ 1 end)))
  (define pwds (map to-list nums))
  (length (filter p2-good? pwds)))

(check-equal? (time (day4-p2)) 1145) ;; correct part 2 answer for my input