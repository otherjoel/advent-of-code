#lang debug racket

(require "intcode.rkt"
         sugar/list)

(define input (file->tape "day13-input.txt"))

(define (eq-or-wildcard? a b)
  (or (equal? b #f) (equal? a b)))
 
(define (make-masker truple-mask)
  (lambda (lst)
    (andmap eq-or-wildcard? lst truple-mask)))

(define scoreboard? (make-masker '(-1 0 #f)))
(define ball-pos? (make-masker '(#f #f 4)))
(define paddle-pos? (make-masker '(#f #f 3)))
(define block? (make-masker '(#f #f 2)))

(define (outputs tape)
  (define t (make-thread tape 'A))
  (thread-send t (current-thread))
  (let loop ([outputs null])
    (match (thread-try-receive)
      [#f (if (thread-running? t) (loop outputs) (reverse outputs))]
      [(var x) (loop (cons x outputs))])))

(define (day13-p1)
  (define tiles (slice-at (outputs input) 3))
  (length (filter block? tiles)))

(define (process-outputs! T state final-output?)
  (when (thread-running? T)
  (let loop ()
    (define x (thread-receive))
    (define y (thread-receive))
    (define val (thread-receive))
    (define cur-output (list x y val))
    (when (paddle-pos? cur-output) (hash-set! state 'paddle x))
    (when (ball-pos? cur-output) (hash-set! state 'ball x))
    (hash-set! state (make-rectangular x y) val)
    (cond [(or (scoreboard? cur-output) (final-output? cur-output)) (void)]
          [else (loop)]))))

;; would. you. like. to. play. a. game
(define (play-game)
  (define game-prog (vector-copy input))
  (vector-set! game-prog 0 2)
  (define T (make-thread game-prog 'ARKANOID))
  (thread-send T (current-thread))
  (define game-state (make-hash))

  ;; last output in the first set of outputs will be the score
  (process-outputs! T game-state scoreboard?)
  (let loop ([hits 0])
    (cond
      [(or (equal? 200 hits)
           (not (thread-running? T))) game-state]
      [else
       (define ball-x (hash-ref game-state 'ball))
       (define paddle-x (hash-ref game-state 'paddle))
       (define cur-score (hash-ref game-state -1+0i))
       (thread-send T (sgn (- ball-x paddle-x)))
       (process-outputs! T game-state ball-pos?)
       (loop (+ hits (sgn (- (hash-ref game-state -1+0i) cur-score))))]))
  (hash-ref game-state -1))
