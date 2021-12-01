#lang debug racket

(require "intcode.rkt"
         rackunit
         sugar/list) ;; From Day 9

(define input (file->tape "day11-input.txt"))

;; Cribbed from my 2018 Day 13 solution
;; I define a DIRECTION in terms of its effect on the coordinates:
;; adding a direction to a coordinate produces the new coordinate one
;; step in the given direction.
;; A COMPASS is a list of DIRECTIONs in counter-clockwise order, with
;; current direction in 1st position.
;; Rotate the list one way or the other == turning direction.
(define cardinal-dirs '(0+1i -1-0i 0-1i 1+0i))

(define turn-left (curryr shift-left-cycle 1))
(define turn-right (curryr shift-cycle 1))

(define BLACK 0)
(define WHITE 1)

(define (test-robot tape [start-color BLACK])
  (define intcode (make-thread tape 'A))
  (thread-send intcode (current-thread))
  (let loop ([compass cardinal-dirs]
             [hull-panels (hash 0 start-color)]
             [painted-panels null]
             [current-loc 0])
    (cond
      [(not (thread-running? intcode))
       (values (remove-duplicates painted-panels)
               hull-panels)]
      [else
       (thread-send intcode (hash-ref hull-panels current-loc BLACK))
       (let* ([new-color (thread-receive)]
              [turn-func (if (zero? (thread-receive)) turn-left turn-right)]
              [turned-compass (turn-func compass)]
              [new-loc (+ (car turned-compass) current-loc)])
         (loop turned-compass
               (hash-set hull-panels current-loc new-color)
               (cons current-loc painted-panels)
               new-loc))])))

(define-values (painted-p1 _) (time (test-robot input)))

(check-equal? (length painted-p1)
              1785) ; answer for part 1

(define (view hull)
  (define squares (hash-keys hull))
  (define low-y (imag-part (argmin imag-part squares)))
  (define hi-y (imag-part (argmax imag-part squares)))
  (define low-x (real-part (argmin real-part squares)))
  (define hi-x (real-part (argmax real-part squares)))
  (apply string-append
         ;; Got to do this backwards
         (for*/list ([y (in-range hi-y (- low-y 1) -1)]
                     [x (in-range low-x (+ 1 hi-x))])
                    (define c (hash-ref hull (make-rectangular x y) BLACK))
                    (string-append (if (equal? BLACK c) "▓" "░")
                                   (if (equal? x hi-x) "\n" "")))))


(define-values (painted-2 registration) (time (test-robot input WHITE)))

(check-equal?
 (view registration)
"▓░▓▓░▓▓▓░░▓▓░░▓▓░▓▓▓▓▓▓░░▓░░░░▓░░░░▓░▓▓░▓▓▓
▓░▓▓░▓▓▓▓░▓░▓▓░▓░▓▓▓▓▓▓▓░▓▓▓▓░▓░▓▓▓▓░▓▓░▓▓▓
▓░░░░▓▓▓▓░▓░▓▓░▓░▓▓▓▓▓▓▓░▓▓▓░▓▓░░░▓▓░░░░▓▓▓
▓░▓▓░▓▓▓▓░▓░░░░▓░▓▓▓▓▓▓▓░▓▓░▓▓▓░▓▓▓▓░▓▓░▓▓▓
▓░▓▓░▓░▓▓░▓░▓▓░▓░▓▓▓▓░▓▓░▓░▓▓▓▓░▓▓▓▓░▓▓░▓▓▓
▓░▓▓░▓▓░░▓▓░▓▓░▓░░░░▓▓░░▓▓░░░░▓░▓▓▓▓░▓▓░▓▓▓
")