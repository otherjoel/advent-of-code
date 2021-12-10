#lang racket/base

(require "aoc.rkt"
         racket/file
         racket/function
         racket/list
         racket/match
         racket/set
         racket/string)

(define (parse/lazy lines)
  (for/list ([line (in-list lines)])
    (map string-length (string-split (cadr (string-split line "|"))))))

(define input/lazy (parse/lazy (file->lines "day08.txt")))

(define (count/1478 lst)
  (for/sum ([outvals (in-list lst)])
    (count (λ (n) (member n '(2 3 4 7))) outvals)))

(define (part-1) (count/1478 input/lazy))

(module+ test (check-answer/ns part-1 539)) ; part-1: 539 (42 μs)

(struct display (wiremap outputs) #:transparent)

(define (str->wires str)
  (list->seteq (filter (λ (s) (not (eq? s '||))) (map string->symbol (string-split str "")))))

(define (find-set lst segment-count [proc (λ (s) #t)]) 
  (car (filter (λ (s) (and (eqv? (set-count s) segment-count) (proc s))) lst)))

(define (contains s) (lambda (s2) (subset? s s2)))
(define (isnt . x) (lambda (s) (not (member s x))))

(define (infer-wiremap samples)
  (match-define (list One Four Seven Eight) (map (curry find-set samples) '(2 4 3 7)))
  (let*
      ([top    (set-subtract Seven One)]
       [Nine   (find-set samples 6 (contains (set-union Four top)))]
       [bottom (set-subtract Nine  (set-union Four Seven))]
       [Three  (find-set samples 5 (contains (set-union Seven bottom)))]
       [middle (set-subtract Three (set-union Seven bottom))]
       [Zero   (set-subtract Eight middle)]
       [Six    (find-set samples 6 (isnt Zero Nine))]
       [upleft (set-subtract Four  (set-union One middle))]
       [Five   (find-set samples 5 (λ (s) (and ((isnt Three) s) ((contains upleft) s))))]
       [Two    (find-set samples 5 (isnt Three Five))])
    (for/hash [(s (in-list (list Zero One Two Three Four Five Six Seven Eight Nine)))
               (i (in-naturals))]
      (values s i))))

(define input (file->lines "day08.txt"))

(define (parse line)
  (match (string-split line "|")
    [(list samples outputs)
     (display (infer-wiremap (map str->wires (string-split samples)))
              (map str->wires (string-split outputs)))]))

(define (decode disp)
  (apply +
         (for/list ([output (in-list (display-outputs disp))]
                    [i (in-list '(1000 100 10 1))])
           (* i (hash-ref (display-wiremap disp) output)))))

(define (sum-outputs displays) (apply + (map decode displays)))

(define (part-2)
  (sum-outputs (map parse input)))

(module+ test (check-answer/ns part-2 1084606)) ; part-2: 1084606 (9588 μs)

(module+ test
  (define test-input
    '("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
      "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
      "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
      "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
      "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
      "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
      "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
      "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
      "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
      "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))

  (check-equal? (count/1478 (parse/lazy test-input)) 26)
  (check-equal? (apply + (map decode (map parse test-input))) 61229))

