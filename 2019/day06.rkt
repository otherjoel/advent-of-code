#lang racket/base

(require racket/match
         racket/file
         rackunit)

(define (parse-line str)
  (match (regexp-match #px"([A-Z0-9]+)\\)([A-Z0-9]+)" str)
    [(list _ orbit-ctr orbiter) (cons orbiter orbit-ctr)]
    [_ #f]))

(define (record-orbits line-strs)
  (make-immutable-hash (map parse-line line-strs)))

(define (count-orbits h)
  (for/sum ([k (in-hash-keys h)])
    (let loop ([next-key (hash-ref h k #f)]
               [orbit-count 0])
      (cond [next-key (loop (hash-ref h next-key #f) (+ 1 orbit-count))]
            [else orbit-count]))))

(define t1 '("COM)B"
             "B)C"
             "C)D"
             "D)E"
             "E)F"
             "B)G"
             "G)H"
             "D)I"
             "E)J"
             "J)K"
             "K)L"))

(check-equal? (time (count-orbits (record-orbits t1))) 42)

(define input (record-orbits (file->lines "day06-input.txt")))

(check-equal? (count-orbits input) 417916) ;; part 1 answer

;; Walk up the branches of the orbit “tree” in parallel, starting from both your
;; and Santa’s positions, and for each, record how many hops it took to get to the
;; current planets from their starting planets. If either one comes across a planet
;; the other’s path has already reached, add the current number of hops to the number
;; of hops it took them to get there, and there’s your answer.
(define (hops-between h your-planet santa-planet)
  (let loop ([you-trace your-planet]
             [santa-trace santa-planet]
             [you-seen (hash your-planet 0)]
             [santa-seen (hash santa-planet 0)]
             [hops 1])
    (define next-you (hash-ref h you-trace #f))
    (define next-santa (hash-ref h santa-trace #f))
    (define you-seen-now
      (cond [next-you (hash-set you-seen next-you hops)]
            [else you-seen]))
    (define santa-seen-now
      (cond [next-santa (hash-set santa-seen next-santa hops)]
            [else santa-seen]))
    (define on-santas-trail (hash-ref santa-seen-now next-you #f))
    (define on-your-trail (hash-ref you-seen-now next-santa #f))
    (cond
      [on-santas-trail (+ hops on-santas-trail)]
      [on-your-trail (+ hops on-your-trail)]
      [else (loop next-you next-santa you-seen-now santa-seen-now (+ hops 1))])))

;; part 2 answer
(check-equal?
 (time (hops-between input (hash-ref input "YOU") (hash-ref input "SAN")))
 523)