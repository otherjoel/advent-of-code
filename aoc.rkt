#lang racket/base

(require (for-syntax racket/base syntax/parse))
(require monotonic rackunit)

(provide (all-defined-out)
         (all-from-out rackunit))

(define-syntax (check-answer stx)
  (syntax-case stx ()
    [(_ func answer)
     #`(test-begin
        (define-values (results cpu clock gc) (time-apply func '()))
        #,(syntax/loc stx (check-equal? (car results) answer))
        (displayln (format "~a: ~a (cpu: ~a real: ~a gc: ~a)" 'func (car results) cpu clock gc)))]))

(define (time-proc/ns thunk [rounds 1])
  (for/fold ([res #f]
             [dur-lst '()]
             #:result (values res (round (/ (apply + dur-lst) rounds))))
            ([i (in-range rounds)])
    (when (> i 0)
      (collect-garbage)
      (collect-garbage)
      (collect-garbage))
    (let* ([start (nanotime)]
           [results (thunk)]
           [dur (- (nanotime) start)])
      (values results (cons dur dur-lst)))))

(define (fmt-ns ns)
  (cond
    [(> ns 1000000) (format "~a ms" (quotient ns 1000000))]
    [(> ns 1000) (format "~a μs" (quotient ns 1000))]
    [else (format "~a ns" ns)]))

(define (display-time/ns thunk [rounds 1] [who (object-name thunk)])
  (define-values (results avg-ns) (time-proc/ns thunk rounds))
  (displayln (format "~a: ~a (~a)" who results (fmt-ns avg-ns))))

(define (time-only/ns thunk [rounds 1])
  (define-values (_results avg-ns) (time-proc/ns thunk rounds))
  (displayln (format "~a" (fmt-ns avg-ns))))

(define-syntax (check-answer/ns stx)
  (syntax-parse stx
    [(check-answer/ns proc:id answer:expr (~optional rounds:expr))
     #`(test-begin
        (define-values (results avg-ns) (time-proc/ns proc (~? rounds 1)))
        #,(syntax/loc stx (check-equal? results answer))
        (displayln (format "~a: ~a (~a)" 'proc results (fmt-ns avg-ns))))]))

