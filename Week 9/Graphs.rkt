#lang racket

(define (makeHash f vals)
  (map (lambda (x) (cons x (f x))) vals))

(define (keys hash)
  (map car hash))

(define (values hash)
  (map cdr hash))

(define hash (makeHash (lambda (x) (* x x)) (range 1 10)))

(define (del-assoc key hash)
  (filter (lambda (pair) (not (equal? key (car pair)))) hash))

(define (add-assoc key value hash)
  (cons (cons key value) (del-assoc key hash)))

(define (search p list)
  (and (not (null? list))
       (or (p (car list))
           (search p (cdr list)))))

(define (assoc key hash)
  (search (lambda (pair) (and (equal? (car pair) key) pair)) hash))

(define G '((1 2 3) (2 3 6) (3 4 6) (4 1 5) (5 3) (6 5)))

(define vertices keys)

(define (foldl op result list)
  (if (null? list)
      result
      (foldl op (op result (car list)) (cdr list))))

(define (edges G)
  (define (collectEdges u G)
    (foldl
     (lambda (result v)
       (cons (cons u v) result))
     '()
     (children u G)))
  (foldl
   (lambda (result v)
     (append (collectEdges v G) result))
   '()
   (vertices G)))

(define (children v G)
  (cdr (assoc v G)))

(define (edge? u v G)
  (memv v (children u G)))

(define (mapChildren f u G)
  (map f (children u G)))

(define (searchChild p u G)
  (search p (children u G)))

(define (childless G)
  (filter (lambda (v) (null? (children v G))) (vertices G)))

(define (parents v G)
  (filter (lambda (u) (edge? u v G) ) (vertices G)))

(define (all p list)
  (not (search (lambda (x) (not (p x))) list)))

(define (hasVertex v G)
  (and (memv v (vertices G)) #t))

(define (insertVertex v G)
  (if (hasVertex v G)
      G
      (add-assoc v '() G)))

(define (removeVertex v G)
  (define (notV x) (not (equal? x v)))
  (define (filterChildren u)
    (filter notV (children u G)))
  (if (hasVertex v G)
      (let ([rest (filter notV (vertices G))])
        (map
         (lambda (u)
           (cons u (filterChildren u)))
         rest))
       G))

(define (symmetric? G)
  (all
   (lambda (u)
     (all
      (lambda (v) (edge? v u G))
      (children u G)))
     (vertices G)))

(define (dfsPath u v G)
  (define (dfs-search path)
    (let ([current (car path)])
      (cond
        [(equal? current v) (reverse path)]
        [(memv current (cdr path)) #f]
        [else (searchChild
               (lambda (w) (dfs-search (cons w path)))
               current
               G)]
       )))
  (dfs-search (list u)))

(define (bfsPath u v G)
  
  (define (extendPath path)
    (map (lambda(w) (cons w path))
         (children (car path) G)))
  (define (remains-acyclic? path)
    (not (memv (car path) (cdr path))))
  (define (extendAcyclic path)
    (filter remains-acyclic? (extendPath path)))
 
  (define (extendLevel level)
    (apply append (map extendAcyclic level)))
  (define (targetPath path)
    (and (equal? (car path) v) (reverse path)))
  (define (bfsLevel level)
    (and (not (null? level))
         (or (search targetPath level)
             (bfsLevel (extendLevel level)))))

  (bfsLevel (list (list u))))

(define (add-if-missing x list)
  (if (member x list)
      list
      (cons x list)))

(define (addEdge u v G)
  (let ([G-with-u-v (insertVertex v (insertVertex u G))])
    (add-assoc u (add-if-missing v (children u G-with-u-v)) G-with-u-v)))

(define (removeEdge u v G)
  (add-assoc u
             (filter
              (lambda (w) (not (equal? w v)))
              (children u G))
             G))

(define (out-degree v G)
  (length (children v G)))

(define (in-degree u G)
  (foldl + 0 (map (lambda (v) (if (edge? v u G) 1 0)) (vertices G))))

(define (acyclic? G)
  (define (dfs path)
    (let ([current (car path)])
      (or (member current (cdr path))
          (searchChild 
           (lambda (child) (dfs (cons child path)))
           current
           G))))
  (not (search
        (lambda (v) (dfs (list v)))
        (vertices G))))

(define (makeEmptyGraph vertices)
  (map (lambda (v) (cons v '())) vertices))

(define (invert G)
  (foldl
   (lambda (result edge) (addEdge (cdr edge) (car edge) result))
   (makeEmptyGraph (vertices G))
   (edges G)))