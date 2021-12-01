#lang racket

(require rackunit)

(define input (string-trim (file->string "day05-input.txt")))

(define (ascii-vector str)
  (list->vector (map char->integer (string->list str))))

(define input-vec (ascii-vector input))

(define (ascii-at-pos str i)
  (char->integer (list-ref (string->list str) i)))

;; The ASCII values of characters of alternating case ("aA" or "Gg") will always differ by 32
;; Return the first such occurrence or "" if none exists
(define (first-alternating-case str)
  (or
   (for/first ([i (in-range (sub1 (string-length str)))]
               #:when (= 32 (abs (- (ascii-at-pos str i)
                                    (ascii-at-pos str (add1 i))))))
              (substring str i (+ i 2)))
   ""))

;; Eliminate the first pair of characters that differ only in case
(define (next-transformation str)
  (or (string-replace str (first-alternating-case str) "") str))

;; Vector versions
(define (next-nonzero vec i)
  (or 
   (for/first ([n (in-range (min (add1 i) (vector-length vec)) (vector-length vec))]
               #:when (positive? (vector-ref vec n)))
              n)
   i))

(define (vec-first-alternating-case vec)
  (or 
   (for/first ([i (in-range (vector-length vec))]
               #:when (and (positive? (vector-ref vec i))
                           (= 32 (abs (- (vector-ref vec i)
                                         (vector-ref vec (next-nonzero vec i)))))))
              (list i (next-nonzero vec i)))
   '()))

(define (vec-next-transform! vec)
  (define next-pair (vec-first-alternating-case vec))
  (cond
    [(not (empty? next-pair))
     (for ([i (in-list next-pair)])
          (vector-set! vec i 0))
     #t]
    [else #f]))

;; Keep transforming as above until there's nothing left to do
(define (boil str)
  (for/fold ([last-state ""]
             [current-state str]             
             #:result current-state)
            ([x (in-naturals)]
             #:break (string=? current-state last-state))
    (values current-state (next-transformation current-state))))

;; Let's see if vectors are faster
(define (vector-boil vec)
  (do ([xformed? (vec-next-transform! vec) (vec-next-transform! vec)])
    [(not xformed?)] ))

;; I ran this function and started eating a bowl of chili.
;; When I was done with the chili it hadn't completed yet.
(define (day01-part1-try1)
  (string-length (boil input)))

(define (ascii-vector->string vec)
  (list->string (map integer->char (vector->list (vector-filter positive? vec)))))

(define (day01-part1-try2)
  (vector-boil input-vec)
  (vector-length (vector-filter positive? input-vec)))

(module+ test
  (check-equal? (boil "dabAcCaCBAcCcaDA") "dabCBAcaDA")

  ;; Takes about 64sec on my laptop
  (check-equal? (time (day01-part1-try2)) 10584)) ; Correct answer for part one