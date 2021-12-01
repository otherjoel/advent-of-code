#lang debug racket

(require sugar/coerce
         rackunit
         threading)

;; IntCode programs are referred to as “tapes”
;; Tapes can be run three ways:
;;   1. Interactively, using the provided `run-tape` function
;;   2. As functions, with inputs provided as arguments
;;   3. As threads, with inputs provided via `thread-send`

(define last-output (make-parameter null))
(define input-fn (make-parameter read))
(define output-fn (make-parameter void))

;; Debugging instrumentation
(define (tape-id type v) (format "[~a ~a]" type v))
(define current-tape (make-parameter (tape-id 'main 0)))
(define debug? (make-parameter #f))
(define (log val) (when (debug?) (eprintf "~a ~a~n" (current-tape) val)))

;; The IntCode Instruction Set ---------------------
;;
(struct instruction (func inputs outputs argmodes) #:transparent)
(struct jump-result (val) #:transparent)

(define (op-output x)
  (log x)
  (last-output x)
  ((output-fn) x))

(define (op-input)
  (define datum ((input-fn)))
  (cond [(eof-object? datum) (raise 'NOT-ENOUGH-INPUTS)]
        [else datum]))

(define (make-binary proc) (lambda (a b) (if (proc a b) 1 0)))
(define (make-jumper proc) (lambda (v dest) (if (proc v) (jump-result dest) 0)))

(define jump-if-true (make-jumper positive?))
(define jump-if-false (make-jumper zero?))
(define less-than? (make-binary <))
(define equals? (make-binary equal?))

(define opcodes
  (hash 1 (instruction + 2 1 null)
        2 (instruction * 2 1 null)
        3 (instruction op-input 0 1 null)
        4 (instruction op-output 1 0 null)
        5 (instruction jump-if-true 2 0 null)
        6 (instruction jump-if-false 2 0 null)
        7 (instruction less-than? 2 1 null)
        8 (instruction equals? 2 1 null)))

;; 1 = literal (“immediate”) mode
;; 0 = pointer (“position”) mode
(define (str->modes str)
  (let* ([s (if (non-empty-string? str) str "0")]
         [m (string->number s 2)])
    (list (bitwise-and m 1)
          (bitwise-and m 2)
          (bitwise-and m 4))))

(define (num->instruction n)
  (match-define (list _ modes-str op-str)
    (or (regexp-match #px"(\\d*)([\\d]{2})" (number->string n))
        (list 0 "0" (number->string n))))
  (struct-copy instruction
               (hash-ref opcodes (string->number op-str))
               [argmodes (str->modes modes-str)]))

;; Do the next instruction at ptr and return a pointer to the next instruction.
(define (do-op! vec ptr)
  (unless (< ptr (vector-length vec)) (raise 'INVALID-CURSOR))
  (match (vector-ref vec ptr)
    [99 -1]
    [(var op)
     (match-define (instruction fn inputs outputs argmodes) (num->instruction op))
     (define params
       (for/list ([idx (in-range inputs)]
                  [mode (in-list (take argmodes inputs))])
         (define arg (vector-ref vec (+ 1 ptr idx)))
         (cond [(zero? mode) (vector-ref vec arg)]
               [else arg])))
     (define result (apply #R fn #R params))
     (cond [(jump-result? result)
            (jump-result-val result)]
           [(positive? outputs)
            (vector-set! vec (vector-ref vec (+ ptr inputs outputs)) result)
            (+ 1 ptr inputs outputs)]
           [else
            (+ 1 ptr inputs outputs)])]))

(define (run-tape vec)
  (with-handlers ([exn:fail? (lambda (e) (log (exn-message e)) (last-output))]
                  [symbol? log])
    (let loop ([ptr 0])
      (match (do-op! vec ptr)
        [-1 (last-output)]
        [(var next-op) (loop next-op)]))))

;; Returns a function that runs a fresh copy of the original tape each time
;; it is called. Any arguments to the function are used to satisfy INPUT ops.
(define (functionize vec)
  (lambda inputs
    (define inputs-str (string-join (map ->string inputs)))
    (parameterize ([current-input-port (open-input-string inputs-str)])
      (run-tape (vector-copy vec)))))

(define (string->tape str)
  (~>> (string-split str ",")
       (map string->number)
       list->vector))

(define input
  (~> (file->string "day07-input.txt")
      string-trim
      string->tape))

(define (thruster-signal tape phases)
  (for/fold ([last-output 0])
            ([phase (in-list phases)])
    ((functionize tape) phase last-output)))

(define (day7-p1)
  (apply max
    (for/list ([phase-setting (in-list (permutations (range 0 5)))])
      (thruster-signal input phase-setting))))

(check-equal? (time (day7-p1)) 116680) ; answer for part 1

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
     (define output-thread (thread-receive))
     (parameterize ([input-fn thread-receive]
                    [output-fn (lambda (v) (thread-send output-thread v))]
                    [current-tape (tape-id 'thread id)])
       (run-tape v))
     (thread-send main-thread (cons id (last-output))))))

(define (feedback-thruster-signal tape phases)
  (define amp-count (length phases))
  (define amps (for/list ([n (in-range amp-count)]) (make-thread tape n)))

  ;; Hook them up in a circle
  (for ([thread/n (in-list (drop-right amps 1))]
        [thread/n+1 (in-list (drop amps 1))])
    (thread-send thread/n thread/n+1))
  (thread-send (last amps) (first amps))
  
  ;; Send each amp its phase (satisfy the first input op)
  (for ([phase (in-list phases)]
        [amp (in-list amps)])
    (thread-send amp phase))

  ;; Send the first amp a value for its second input op
  (thread-send (first amps) 0)

  ;; Call me when ya done
  (for/last ([n (in-range amp-count)])
            (cdr (thread-receive))))

(define (day5-p2)
  (apply max
    (for/list ([phase-setting (in-list (permutations (range 5 10)))])
      (feedback-thruster-signal input phase-setting))))

(check-equal? (time (day5-p2)) 89603079) ;; answer for part 2

(define t1 (string->tape "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"))
(define t2 (string->tape "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"))
(define t3 (string->tape "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"))
(define t4 (string->tape "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"))
(define t5 (string->tape "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"))
