#lang racket

(define (quickSort list)
  (define (genPredicate p)
    (lambda (x) (p x (car list))))
  (if (or (null? list)
          (null? (cdr list)))
      list
      (append
       (quickSort (filter (genPredicate <) list))
       (filter (genPredicate =) list)
       (quickSort (filter (genPredicate >) list)))))

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))

(define emptyTree '())

(define (makeLeaf value)
  (list value emptyTree emptyTree))

(define (makeTree root left right)
  (list root left right))

(define rootTree car)
(define leftTree cadr)
(define rightTree caddr)
(define emptyTree? null?)

(define t
  (makeTree 10
             (makeTree 7
                        (makeLeaf 100)
                        (makeLeaf 2))
             (makeTree 3
                        (makeTree 4
                                   (makeLeaf 1)
                                   (makeLeaf 2))
                        emptyTree)))

(define (treeSum t)
  (if (emptyTree? t)
      0
      (+ (rootTree t)
         (treeSum (leftTree t))
         (treeSum (rightTree t)))))


(define (treeMax t)
  (if (emptyTree? t)
      -inf.0
      (max (rootTree t)
           (treeMax (leftTree t))
           (treeMax (rightTree t)))))

(define (treeMin t)
  (if (emptyTree? t)
      +inf.0
      (min (rootTree t)
           (treeMin (leftTree t))
           (treeMin (rightTree t)))))

(define (-- n) (- n 1))

(define (++ n) (+ n 1))

(define (treeLevel k t)
  (cond
    [(emptyTree? t) '()]
    [(= k 0) (list (rootTree t))]
    [else (append
           (treeLevel (-- k) (leftTree t))
           (treeLevel (-- k) (rightTree t)))]))

(define (depth t)
  (if (emptyTree? t)
      0
      (+ 1 (max (depth (leftTree t))
                (depth (rightTree t))))))

(define (accumulate-r op nv f start end next stop?)
  (if (stop? start end)
      nv
      (op (f start) (accumulate-r op nv f (next start) end next stop?))))

(define (allLevels t)
  (accumulate-r cons '()
              (lambda (i) (treeLevel i t))
              0 (-- (depth t))
              ++ >))

(define (mapTree f t)
  (if (emptyTree? t)
      t
      (makeTree (f (rootTree t))
                (mapTree f (leftTree t))
                (mapTree f (rightTree t)))))

(define (tree->list t)
  (if (emptyTree? t)
      '()
      (append
        (tree->list (leftTree t))
        (cons (rootTree t)
              (tree->list (rightTree t))))))

(define (member x t)
  (cond
    [(emptyTree? t) #f]
    [(equal? x (rootTree t)) t]
    [else (or (member x (leftTree t))
              (member x (rightTree t)))]))

(define (cons#f h t) (and t (cons h t)))

(define (path x t)
  (cond
    [(emptyTree? t) #f]
    [(equal? x (rootTree t)) (list x)]
    [else (cons#f (rootTree t)
                  (or (path x (leftTree t))
                      (path x (rightTree t))))]))

(define (bstInsert val t)
  (cond
    [(emptyTree? t) (makeLeaf val)]
    [(< val (rootTree t)) (makeTree
                            (rootTree t)
                            (bstInsert val (leftTree t))
                            (rightTree t))]
    [else (makeTree (rootTree t)
                    (leftTree t)
                    (bstInsert val (rightTree t)))]))

(define (foldr op nv list)
  (if (null? list)
      nv
      (op (car list) (foldr op nv (cdr list)))))

(define (list->tree list)
  (foldr bstInsert emptyTree list))

(define (treeSort list)
  (tree->list (list->tree list)))
