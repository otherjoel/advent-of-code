#lang racket/base

(require sugar/list
         racket/list
         racket/file
         racket/string
         racket/function
         rackunit
         pict)

(define (string->data str)
  (for/list ([c (in-string str)])
    (string->number (string c))))

(define (image-layers data width height)
  (slice-at data (* width height)))

(define (counter n)
  (lambda (lst) (count (curry equal? n) lst)))

(define (verify-layers lst)
  (define min-zeros (apply min (map (counter 0) lst)))
  (define fewest-zeros
    (for/or ([layer (in-list lst)])
      (cond [(equal? min-zeros ((counter 0) layer)) layer]
            [else #f])))
  (* ((counter 1) fewest-zeros)
     ((counter 2) fewest-zeros)))

(define input (string->data (string-trim (file->string "day08-input.txt"))))

(define (day08-p1)
  (verify-layers (image-layers input 25 6)))

(check-equal? (day08-p1) 2016) ; part 1

(define (final-pixel pixels)
  (for/or ([p (in-list pixels)])
    (cond [(not (equal? 2 p)) p]
          [else #f])))

(define (resolve-layers lst)
  (for/list ([n (in-range (length (first lst)))])
    (final-pixel (map (curryr list-ref n) lst))))

(define input-layers (image-layers input 25 6))

(define result (slice-at (resolve-layers input-layers) 25))

(define (pixel v)
  (if (zero? v) (filled-rectangle 4 4) (rectangle 4 4)))

(define (pixel-row lst)
  (apply hc-append (map pixel lst)))

(define (show-image layers)
  (apply vc-append (map pixel-row layers)))
  
(show-image result) ; part 2 run it in DrRacket and read the output with your eyeballs