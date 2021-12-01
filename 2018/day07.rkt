#lang racket

(require rackunit sugar/debug)

(define input (file->lines "day07-input.txt"))

; A "step rule" is a list of the form `(prerequisite prereq-for)
(define (inputlines->steprules str)
  (rest (regexp-match #px"Step ([A-Z]) must be finished before step ([A-Z]) can begin." str)))

(define steprules (map inputlines->steprules input))

;; A hash table: for each step, a list of prerequisite steps
(define starting-prereqs
  (for/fold ([result (hash)])
            ([steprule (in-list steprules)])
    (let ([existing-prereqs (hash-ref result (second steprule) '())])
      (hash-set result (second steprule) (cons (first steprule) existing-prereqs)))))

(define alphabet
  (map (lambda(c) (list->string (list c))) ; Convert #\A to "A"
       (map integer->char (range 65 91))))

;; The starting step is the one that’s NOT in `rule-dependencies`
;; -- because, by definition, the first step has no dependencies.
(define starting-step
  (first (remove* (hash-keys starting-prereqs) alphabet)))

;; Return the first item in a list of steps that have no remaining prerequisites
;; sorted alphabetically
(define (next-available-step h)
  (define avail-steps
    (sort (filter identity (hash-map h (lambda (k v) (if (empty? v) k #f)))) string<?))
  (cond [(not (empty? avail-steps)) (first avail-steps)]
        [else '()]))

;; Remove the given step from all prerequisite-lists for other tasks, then
;; remove its own prerequisite-list from the hash table
(define (remove-from h step)
  (define updated-prereqs
    (for/hash ([(k v) (in-hash h)])
              (values k (remove step v))))
  (hash-remove updated-prereqs step))

(define (day07-part1)
  (let loop ([prereqs starting-prereqs]
             [current-step starting-step]
             [done-steps '()])
    (cond
      [(hash-empty? prereqs) (apply string-append (reverse done-steps))]
      [else
       (let* ([next-prereqs (remove-from prereqs current-step)]
              [next-step (next-available-step next-prereqs)])
         (loop next-prereqs next-step (cons current-step done-steps)))])))

(module+ test
  (check-equal? (day07-part1) "BGJCNLQUYIFMOEZTADKSPVXRHW")) ; Correct answer for part 1

;; The worker list is a hash table. Each worker is an entry in the hash which contains
;; a list of the form '(task secs-remaining).
(define starting-workers
  (make-hash '((0 0 0) (1 0 0) (2 0 0) (3 0 0) (4 0 0))))

;; Returns seconds needed to complete a task
(define (secs-to-complete step)
  (+ 60 (- (char->integer (first (string->list step))) 64)))

;; Return all the steps with no prerequisites that are not already being worked on
(define (available-steps h workers)
  (define ready-steps (filter identity (hash-map h (lambda (k v) (if (empty? v) k #f)))))
  (define being-worked-on (filter string? (map first (hash-values workers))))
  (remove* being-worked-on ready-steps))

;; Assign any newly-ready steps to available workers, return the new worker list
(define (assign-tasks workers prereqs)
  (define ready-steps (available-steps prereqs workers))
  (define available-workers
    (for/list ([(worker task) (in-hash workers)]
               #:when (not (string? (first task))))
              worker))
  (cond [(and (not (empty? ready-steps)) (not (empty? available-workers)))
         (for ([worker (in-list available-workers)]
               [task (in-list ready-steps)])
              (hash-set! workers worker (list task (secs-to-complete task))))])
  workers)

;; Subtract 1 sec from time remaining for any workers with active tasks
(define (tick workers)
  (for ([(worker task) (in-hash workers)])
       (match-let ([(list task-id secs-left) task])
         (cond [(> secs-left 0)
                (hash-set! workers worker (list task-id (sub1 secs-left)))])))
  workers)

;; For any tasks that have hit 0 secs remaining, remove them from the list of
;; prerequisite tasks, and signal that the worker is now free for a new task
;; (by assigning her a task of `0`)
(define (update prereqs workers)
  (for/fold ([result-prereqs prereqs])
            ([(worker task) (in-hash workers)])
    (cond [(and (equal? (second task) 0) (string? (first task)))
           (begin
             (hash-set! workers worker '(0 0))
             (remove-from result-prereqs (first task)))]
          [else result-prereqs])))

;; Bring it all together now
(define (day07-part2)
  (let loop ([prereqs (hash-set starting-prereqs starting-step '())]
             [workers starting-workers]
             [sec-elapsed 0])
    (cond [(hash-empty? prereqs) sec-elapsed]
          [else (let ([next-workers (tick (assign-tasks workers prereqs))])
                  (loop (update prereqs next-workers) next-workers (add1 sec-elapsed)))])))

(module+ test
  (check-equal? (day07-part2) 1017)) ; Correct answer for part 2