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
              