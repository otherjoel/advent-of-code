#lang racket/base

(require "../aoc.rkt"
         (for-syntax racket/base)
         racket/file
         racket/list
         racket/match
         racket/require
         threading)

(require (filtered-in
            (lambda (name)
              (and (regexp-match #rx"^unsafe-fx" name)
                   (regexp-replace #rx"unsafe-" name "")))
            racket/unsafe/ops))

(struct pos (x y) #:transparent)

(define (parse lines)
  (for/list ([str (in-list lines)])
    (~> (regexp-match #rx"([0-9]+),([0-9]+)" str)
        cdr
        (map string->number _)
        (apply pos _))))

(define (area coord-pair)
  (match-define (list (pos ax ay) (pos bx by)) coord-pair)
  (fx* (fx+ 1 (fxabs (fx- ax bx))) (fx+ 1 (fxabs (fx- ay by)))))

(define input (parse (file->lines "day09-input.txt")))

(define (pt1)
  (~> (combinations input 2)
      (map area _)
      (apply fxmax _)))

(check-answer/ns pt1 4735268538) ; 7ms


;;------------------------------------------------
(struct side (a b) #:transparent)

(define all-sides
  (let loop ([remaining input]
             [len (length input)])
    (if (= 1 len)
        (list (side (car remaining) (car input)))
        (cons (apply side (take remaining 2))
              (loop (cdr remaining) (sub1 len))))))

;; Build sorted vectors for binary search
;; horiz: #(y x-lo x-hi), sorted by y
;; vert:  #(x y-lo y-hi), sorted by x
(define-values (horiz-edges vert-edges)
  (let ()
    (define-values (horiz vert)
      (for/fold ([h '()] [v '()])
                ([s (in-list all-sides)])
        (match-define (side (pos x1 y1) (pos x2 y2)) s)
        (if (fx= y1 y2)
            (values (cons (vector y1 (fxmin x1 x2) (fxmax x1 x2)) h) v)
            (values h (cons (vector x1 (fxmin y1 y2) (fxmax y1 y2)) v)))))
    (values (list->vector (sort horiz fx< #:key (λ (v) (vector-ref v 0))))
            (list->vector (sort vert fx< #:key (λ (v) (vector-ref v 0)))))))

;; Binary search: find first index where (vector-ref (vector-ref vec i) 0) >= val
(define (bsearch-ge vec val)
  (let loop ([lo 0] [hi (vector-length vec)])
    (if (fx>= lo hi)
        lo
        (let* ([mid (fxquotient (fx+ lo hi) 2)]
               [v (vector-ref (vector-ref vec mid) 0)])
          (if (fx< v val)
              (loop (fx+ mid 1) hi)
              (loop lo mid))))))

;; Check if any edge in sorted-vec with coord in (lo, hi) has range overlapping (r-lo, r-hi)
(define (any-edge-crosses? sorted-vec lo hi r-lo r-hi)
  (define start (bsearch-ge sorted-vec (fx+ lo 1)))
  (define len (vector-length sorted-vec))
  (let loop ([i start])
    (and (fx< i len)
         (let ([e (vector-ref sorted-vec i)])
           (and (fx< (vector-ref e 0) hi)  ; coord still in range
                (or (and (fx< (vector-ref e 1) r-hi)   ; e's lo < rect's hi
                         (fx> (vector-ref e 2) r-lo))  ; e's hi > rect's lo
                    (loop (fx+ i 1))))))))

(define (rect-crosses-edge? rect-corners)
  (match-define (list (pos ax ay) (pos bx by)) rect-corners)
  (define min-x (fxmin ax bx))
  (define max-x (fxmax ax bx))
  (define min-y (fxmin ay by))
  (define max-y (fxmax ay by))
  (or (any-edge-crosses? horiz-edges min-y max-y min-x max-x)
      (any-edge-crosses? vert-edges min-x max-x min-y max-y)))

(define (pt2)
  (define rects (~> (combinations input 2) (sort fx> #:key area)))
  (for/first ([rect (in-list rects)] #:unless (rect-crosses-edge? rect)) (area rect)))

(check-answer/ns pt2 1537458069) ; 39ms
