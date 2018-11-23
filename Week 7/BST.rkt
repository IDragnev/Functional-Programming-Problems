#lang racket

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3))
           (tree? (cadr t))
           (tree? (caddr t))))

(define empty-tree '())
(define (make-tree root left right) (list root left right))      
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

(define t
  (make-tree 10
             (make-tree 7
                        (make-leaf 10)
                        (make-leaf 2))
             (make-tree 3
                        (make-tree 4
                                   (make-leaf 1)
                                   (make-leaf 2))
                        empty-tree)))

(define (sum-tree t)
  (if (empty-tree? t)
      0
      (+ (root-tree t)
         (sum-tree (left-tree t))
         (sum-tree (right-tree t)))))

(define (tree-max t)
  (if (empty-tree? t)
      -inf.0
      (max (root-tree t)
           (tree-max (left-tree t))
           (tree-max (right-tree t)))))

(define (tree-min t)
  (if (empty-tree? t)
      +inf.0
      (min (root-tree t)
           (tree-min (left-tree t))
           (tree-min (right-tree t)))))

(define (-- n) (- n 1))

(define (tree-level k t)
  (cond
    [(empty-tree? t) empty-tree]
    [(zero? k) (list (root-tree t))]
    [else (append (tree-level (-- k) (left-tree t))
                  (tree-level (-- k) (right-tree t)))]
))

(define (tree-depth t)
  (if (empty-tree? t)
      0
      (+ 1 (max (tree-depth (left-tree t))
                (tree-depth (right-tree t))))))

(define (all-levels t)
  (define (for level result)
    (if (negative? level)
        result
        (for (-- level) (cons (tree-level level t) result))))
  (for (-- (tree-depth t)) '()))

(define (map-tree f t)
  (if (empty-tree? t)
      empty-tree
      (make-tree
       (f (root-tree t))
       (map-tree f (left-tree t))
       (map-tree f (right-tree t)))))

(define (id x) x)

(define (tree->list t)
    (if (empty-tree? t)
        '()
        (append (tree->list (left-tree t))
                (list (root-tree t))
                (tree->list (right-tree t)))))

(define (bst-insert val bst)
  (cond
    [(empty-tree? bst) (make-leaf val)]
    [(< val (root-tree bst)) (make-tree (root-tree bst)
                                        (bst-insert val (left-tree bst))
                                        (right-tree bst))]
    [else (make-tree (root-tree bst)
                     (left-tree bst)
                     (bst-insert val (right-tree bst)))]
))

(define (foldl op result list)
  (if (null? list)
      result
      (foldl op (op result (car list)) (cdr list))))

(define (tree-sort list)
  (tree->list (foldl
               (lambda (result x) (bst-insert x result))
               empty-tree
               list)))

(define (make-rand-stream seed)
  (define (next-random current)
    (remainder (+ (* current 8253729) 2396403) 32767))
  (define rng (stream-cons seed (stream-map next-random rng)))
  rng)