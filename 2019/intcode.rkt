#lang racket/base

(require sugar/coerce
         rackunit
         threading
         racket/file
         racket/match
         racket/list
         racket/function
         racket/string
         racket/vector)

(provide (all-defined-out))

;; IntCode programs are referred to as “tapes”
;; Tapes can be run three ways:
;;   1. Interactively, using the provided `run-tape` function
;;   2. As functions, with inputs provided as arguments
;;   3. As threads, with inputs provided via `thread-send`

(define last-output (make-parameter null))
(define input-fn (make-parameter read))
(define output-fn (make-parameter void))
(define extended-memory (make-parameter (hash)))
(define relative-base (make-parameter 0))

;; Debugging instrumentation
(define (tape-id type v) (format "[~a ~a]" type v))
(define current-tape (make-parameter (tape-id 'main 0)))
(define debug-lev (make-parameter 0))
(define (log lev val)
  (when (>= (debug-lev) lev) (eprintf "~a ~a~n" (current-tape) val)) val)

;; The IntCode Instruction Set ---------------------
;;
(struct instruction (func inputs outputs argmodes) #:transparent)
(struct jump-result (val) #:transparent)

(define (op-output x)
  (log 1 x)
  (last-output x)
  ((output-fn) x))

(define (op-input)
  (define datum ((input-fn)))
  (cond [(eof-object? datum) (raise 'NOT-ENOUGH-INPUTS)]
        [else datum]))

(define (make-binary proc) (lambda (a b) (if (proc a b) 1 0)))
(define (make-jumper proc) (lambda (v dest) (if (proc v) (jump-result dest) 0)))

;; I define these all directly in-place so I see friendlier identifiers
;; when debug-level is 2
(define (jump-if-true v dest) (if (positive? v) (jump-result dest) 0))
(define (jump-if-false v dest) (if (zero? v) (jump-result dest) 0))
(define (less-than? a b) (if (< a b) 1 0))
(define (equals? a b) (if (equal? a b) 1 0))
(define (adjust-rel-base v) (relative-base (+ (relative-base) v)))

(define opcodes
  (hash 1 (instruction + 2 1 null)
        2 (instruction * 2 1 null)
        3 (instruction op-input 0 1 null)
        4 (instruction op-output 1 0 null)
        5 (instruction jump-if-true 2 0 null)
        6 (instruction jump-if-false 2 0 null)
        7 (instruction less-than? 2 1 null)
        8 (instruction equals? 2 1 null)
        9 (instruction adjust-rel-base 1 0 null)))

(define (num->instruction n)
  (define opcode (modulo n 100))
  (define modecode (quotient n 100))
  (define mode-lst
    (let* ([a1 (quotient modecode 100)]
           [a2 (- (quotient modecode 10) (* 10 a1))]
           [a3 (- (quotient modecode 1) (* 100 a1) (* 10 a2))])
      (list a3 a2 a1)))
  (struct-copy instruction
               (hash-ref opcodes opcode)
               [argmodes mode-lst]))

;; Allow access to “extended memory”
;; If the program tries to read or write past the existing vector,
;; use a hash table to store and retrieve the value.
(define (peek vec idx)
  (cond [(<= idx (sub1 (vector-length vec))) (vector-ref vec idx)]
        [else (hash-ref (extended-memory) idx 0)]))

(define (poke! vec idx val)
  (cond [(<= idx (sub1 (vector-length vec))) (vector-set! vec idx val)]
        [else (extended-memory (hash-set (extended-memory) idx val))])) ; the immutable way?

;; Do the next instruction at ptr and return a pointer to the next instruction.
(define (do-op! vec ptr)
  (match (peek vec ptr)
    [99 -1]
    [(var op)
     (match-define (instruction fn inputs outputs argmodes)
       (log 2 (num->instruction (log 2 op))))
     (define params
       (for/list ([idx (in-range inputs)]
                  [mode (in-list (take argmodes inputs))])
         (define arg (peek vec (+ 1 ptr idx)))
         (cond [(zero? mode) (peek vec arg)]
               [(equal? 1 mode) arg]
               [else (peek vec (+ (relative-base) arg))])))
     (define result (apply fn params))
     (define output-mode (list-ref argmodes (+ outputs inputs -1)))
     (define output-addr
       (cond [(zero? output-mode)
              (peek vec (+ ptr inputs outputs))]
             [else (+ (relative-base)
                      (peek vec (+ ptr inputs outputs)))]))
     (log 2 (format "params ~a / outp-mode ~a / out-addr ~a / result ~a"
                    params output-mode output-addr result))
     (cond [(jump-result? result)
            (jump-result-val result)]
           [(and (positive? outputs))
            (poke! vec output-addr result)
            (+ 1 ptr inputs outputs)]
           [else
            (+ 1 ptr inputs outputs)])]))

(define (run-tape vec)
  (extended-memory (hash))
  (relative-base 0)
  (with-handlers (#;[exn:fail? (lambda (e) (log (exn-message e)) (last-output))]
                  [symbol? (curry log 1)])
    (let loop ([ptr 0])
      (match (do-op! vec ptr)
        [-1 (last-output)]
        [(var next-op) (loop next-op)]))))

;; Returns a function that runs a fresh copy of the original tape each time
;; it is called. Any arguments to the function are used to satisfy INPUT ops.
(define (functionize vec id)
  (lambda inputs
    (define inputs-str (string-join (map ->string inputs)))
    (parameterize ([current-input-port (open-input-string inputs-str)]
                   [current-tape (tape-id 'func id)])
      (run-tape (vector-copy vec)))))

(define (string->tape str)
  (~>> (string-split str ",")
       (map string->number)
       list->vector))

(define (file->tape f) (string->tape (string-trim (file->string f))))
;; Starts a tape running in its own thread and returns the thread descriptor.
;; The first thing you send back to the thread must be another thread to use for
;; sending output. After this, each value sent will be used to satisfy INPUT
;; operations. When the program terminates, the value of its last output will
;; be sent to the parent thread.
(define (make-thread vec id)
  (define main-thread (current-thread))
  (define v (vector-copy vec))
  (thread
   (lambda ()
     (current-tape (tape-id 'thread id))
     (log 2 "Asking for output thread descriptor")
     (define output-thread (thread-receive))
     (parameterize ([input-fn thread-receive]
                    [output-fn (lambda (v) (thread-send output-thread v))])
       (run-tape v))
     (unless (eq? output-thread main-thread)
       (thread-send main-thread (cons id (last-output)))))))

;; In case of debugging needs

(define t1 (string->tape "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
(define t2 (string->tape "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"))
(define t3 (string->tape "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))


(define t4 (string->tape "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))
(define t5 (string->tape "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"))

(define d9-a (string->tape "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"))
(define d9-b (string->tape "1102,34915192,34915192,7,4,7,99,0"))