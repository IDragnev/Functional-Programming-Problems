#lang racket

(require  "Associative-lists.rkt")

(define (search p list)
  (and (not (null? list))
       (or (p (car list))
           (search p (cdr list)))))

(define (all? p? list)
  (not (search (lambda (x) (not (p? x))) list)))

(define G '((1 2 3) (2 3 6) (3 4 6) (4 1 5) (5 3) (6 5)))

(define vertices keys)

(define (contains v G)
  (memv v (vertices G)))

(define (children u G)
  (cdr (assqv u G)))

(define (edge? u v G)
  (memv v (children u G)))

(define (mapChildren f u G)
  (map f (children u G)))

(define (searchChild f u G)
  (search f (children u G)))

(define (childlessVertices G)
  (filter (lambda (v) (null? (children v G))) (vertices G)))

(define (parents u G)
  (filter (lambda (v) (edge? v u G)) (vertices G)))

(define (symmetric? G)
  (all? (lambda (u)
          (all? (lambda (v) (edge? v u G)) (children u G)))
   (vertices G)))

(define (insertVertex v G)
  (and (not (contains v G))
       (add-assoc v '() G)))

(define (removeVertex v G)
  (if (contains v G)
      (map (lambda (list)
             (cons (car list)
                   (filter
                    (lambda (x) (not (eqv? x v)))
                    (cdr list))))
           (del-assoc v G))
      G))
   
(define (dfsPath u v G)
  (define (dfs result)
    (let ([current (car result)])
      (cond
        [(eqv? current v) (reverse result)]
        [(memv current (cdr result)) #f]
        [else (search
               (lambda (w) (dfs (cons w result)))
               (children current G))]
       )))
  (dfs (list u)))

(define (bfsPath u v G)
  (define (extend path)
    (map (lambda (v) (cons v path)) (children (car path) G)))
  (define (remainsAcyclic? path) (not (memv (car path) (cdr path))))
  (define (extendAcyclic path) (filter remainsAcyclic? (extend path)))
  (define (targetPath path) (and (eqv? (car path) v) (reverse path)))
  (define (extendLevel level) (apply append (map extendAcyclic level)))                               
  (define (bfs level)
    (and (not (null? level))
         (or (search targetPath level)
             (bfs (extendLevel level)))))
  (bfs (list (list u))))
  
