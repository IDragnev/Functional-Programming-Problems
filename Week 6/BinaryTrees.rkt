#lang racket

(define (++ n) (+ n 1))

(define (tree? t)
  (or (null? t)
      (and (list? t)
           (= (length t) 3)
           (tree? (cadr t))
           (tree? (caddr t)))))

(define emptyTree '())
(define (makeTree root left right)
  (list root left right))

(define root car)
(define leftSubtree cadr)
(define rightSubtree caddr)
(define emptyTree? null?)

(define (depth t)
  (if (emptyTree? t)
      0
      (++ (max (depth (leftSubtree t))
               (depth (rightSubtree t))))))

(define (member x t)
  (cond
    [(emptyTree? t) #f]
    [(equal? x (root t)) t]
    [else (or (member x (leftSubtree t))
              (member x (rightSubtree t)))]
 ))

(define (cons#f h t) (and t (cons h t)))
(define (path x t)
  (cond
    [(emptyTree? t) #f]
    [(equal? x (root t)) (list x)]
    [else (cons#f (root t)
                  (or (path x (leftSubtree t))
                      (path x (rightSubtree t))))]
))