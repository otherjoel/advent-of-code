#lang racket/base

(require (for-syntax racket/base))
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

(define-syntax (check-answer/ns stx)
  (syntax-case stx ()
    [(_ func answer)
     #`(test-begin
        (define-values (results ns)
          (let* ([start (nanotime)]
                 [results (func)]
                 [dur (- (nanotime) start)])
            (values results dur)))
        #,(syntax/loc stx (check-equal? results answer))
        (define duration (if (> ns 1000) (format "~a μs" (quotient ns 1000)) (format "~a ns" ns)))
        (displayln (format "~a: ~a (~a)" 'func results duration)))]))