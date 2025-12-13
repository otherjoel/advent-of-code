#lang rosette

(provide solve-machine-joltage)

(require rosette/lib/angelic)

(define (solve-machine-joltage buttons targets)
  ; Symbolic press count per button
  (define xs (for/list ([_ buttons]) (define-symbolic* x integer?) x))
  
  ; Non-negative
  (for ([x xs]) (assert (>= x 0)))
  
  ; For each counter j, constrain the sum
  (for ([(target j) (in-indexed targets)])
    (define sum
      (foldl + 0
             (for/list ([x xs] [btn buttons] #:when (bitwise-bit-set? btn j))
               x)))
    (assert (= sum target)))
  
  ; Minimize
  (define total (foldl + 0 xs))
  (define sol (optimize #:minimize (list total)
                        #:guarantee (assert #t)))
  
  (evaluate total sol))