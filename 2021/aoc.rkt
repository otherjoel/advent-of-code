#lang racket/base

(require (for-syntax racket/base))
(require rackunit)

(provide (all-defined-out))

(define-syntax (check-answer stx)
  (syntax-case stx ()
    [(_ func answer)
     (with-syntax ([func-name (syntax->datum #'func)])
       #`(test-begin
          (define-values (results cpu clock gc) (time-apply func '()))
          #,(syntax/loc stx (check-equal? (car results) answer))
          (displayln (format "~a: ~a (cpu: ~a real: ~a gc: ~a)" 'func-name (car results) cpu clock gc))))]))