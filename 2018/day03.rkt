#lang racket

(require racket/draw rackunit)

(define input (file->lines "day03-input.txt"))

(define fabric-bmp (make-bitmap 1000 1000 #t))
(define fabric-dc (new bitmap-dc% [bitmap fabric-bmp]))
(define c (make-object color% 0 255 0 0.5))
(define pb (new brush% [color c]))
(define color-probe (make-object color%))

(send fabric-dc set-brush pb)
(send fabric-dc set-pen "white" 0 'transparent) ; don't draw outlines

;; Get the alpha value of pixel x y
(define (get-fabric-value x y)
  (send fabric-dc get-pixel x y color-probe)
  (send color-probe alpha))

;; Parse “claims” of the form "#1 @ 1,3: 4x4" into '(1 1 3 4 4)
(define (parse-claim str)
  (map string->number (rest (regexp-match #px"#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" str))))

;; draw a particular claim onto the fabric map
(define (draw-claim l)
  (send/apply fabric-dc draw-rectangle (rest l)))

;; Returns #t if a pixel's alpha value indicates it’s been painted on more than once
(define (is-overlap? v)
  ;; For some reason the actual alpha of a pixel that gets
  ;; painted with my brush exactly once comes out to this weird number
  (> v 0.5019607843137255))

;; Count the number of pixels with overlap values
(define (count-overlap [startx 0] [starty 0] [width 1000] [height 1000])
  (count is-overlap?
         (for*/list ([x (in-range startx (+ startx width))]
                     [y (in-range starty (+ starty height))])
                    (get-fabric-value x y))))

(define (day03-part1)
  (map draw-claim (map parse-claim input))
  (count-overlap))

(module+ test
  (check-equal? (day03-part1) 100595)) ; Correct answer for part 1

(define (claim-has-overlap? l)
  (> (apply count-overlap (rest l)) 0))

(define (day03-part2)
  (define answer
    (for/first ([c (in-list (map parse-claim input))]
              #:when (not (claim-has-overlap? c)))
             c))
  (highlight-claim answer)
  (send fabric-bmp save-file "day03-results.png" 'png)
  (first answer))

(module+ test
  (check-equal? (day03-part2) 415)) ; Correct answer for part 2

;; Extra

(define (highlight-claim c)
  (send fabric-dc set-brush "red" 'solid)
  (draw-claim c))