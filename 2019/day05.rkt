#lang racket/base

(require racket/match
         racket/list
         racket/string
         racket/file
         threading)

(struct op-info (func inputs outputs) #:transparent)
(struct instruction op-info (argmodes) #:transparent)

(struct jump-result (val) #:transparent)

(define (op-output x)
  (displayln (format "OUTPUT: ~a" x)))

(define (op-input)
  (displayln "INPUT:")
  (string->number (read-line)))

(define (op-end)
  (raise 'END))

(define (jump-if-true v dest)
  (if (positive? v) (jump-result dest) 0))

(define (jump-if-false v dest)
  (if (zero? v) (jump-result dest) 0))

(define (less-than? v1 v2)
  (if (< v1 v2) 1 0))

(define (equals? v1 v2)
  (if (equal? v1 v2) 1 0))

(define opcodes
  (hash 1 (op-info + 2 1)
        2 (op-info * 2 1)
        3 (op-info op-input 0 1)
        4 (op-info op-output 1 0)
        5 (op-info jump-if-true 2 0)
        6 (op-info jump-if-false 2 0)
        7 (op-info less-than? 2 1)
        8 (op-info equals? 2 1)
        99 (op-info op-end 0 0)))

(define (mode v)
  (if (positive? v) 'literal 'pointer))

(define (str->modes str)
  (let* ([s (if (non-empty-string? str) str "0")]
         [m (string->number s 2)])
    (list (mode (bitwise-and m 1))
          (mode (bitwise-and m 2))
          (mode (bitwise-and m 4)))))

(define (num->instruction n)
  (match-define (list _ modes-str op-str)
    (or (regexp-match #px"(\\d*)([\\d]{2})" (number->string n))
        (list 0 "0" (number->string n))))
  (match-define (op-info fn ins outs) (hash-ref opcodes (string->number op-str)))
  (instruction fn ins outs (str->modes modes-str)))

;; Do the next instruction at ptr and return a pointer to the next instruction.
(define (do-op! vec ptr)
  (unless (< ptr (vector-length vec)) (raise 'INVALID-CURSOR))
  (match-define (instruction fn inputs outputs argmodes)
    (num->instruction (vector-ref vec ptr)))
  (define params
    (for/list ([idx (in-range inputs)]
               [mode (in-list (take argmodes inputs))])
      (define arg (vector-ref vec (+ 1 ptr idx)))
      (cond [(equal? 'pointer mode) (vector-ref vec arg)]
            [else arg])))
  (define result (apply fn params))
         
  (cond [(jump-result? result)
         (jump-result-val result)]
        [(equal? 1 outputs)
         (vector-set! vec (vector-ref vec (+ ptr inputs outputs)) result)
         (+ 1 ptr inputs outputs)]
        [else
         (+ 1 ptr inputs outputs)]))

(define (string->program str)
  (~>> (string-split str ",")
       (map string->number)
       list->vector))

(define (run-prog vec)
  (let loop ([ptr 0])
    ;; The <99> instruction raises an exception to end the program
    (with-handlers ([symbol? (lambda (e) (printf "~a" e))])
      (loop (do-op! vec ptr)))))

(define (day5-p1)
  (~> (file->string "day05-input.txt")
      string-trim
      string->program
      run-prog))

(define (run-piped input)
  (define out (open-output-string))
  (parameterize ([current-input-port (open-input-string input)]
                 [current-output-port out])
   (day5-p1))
  (get-output-string out))