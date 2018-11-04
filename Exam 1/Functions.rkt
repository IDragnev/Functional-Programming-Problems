#lang racket

(provide
 accumulate
 filter
 foldl
 foldr
)

(define (accumulate op nv start end f next)
  (if (> start end)
      nv
      (op (f start) (accumulate op nv (next start) end f next))
 ))

(define (filter p? l)
  (cond
    [(null? l) l]
    [(p? (car l)) (cons (car l) (filter p? (cdr l)))]
    [else (filter p? (cdr l))]
))

(define (foldr op nv ls)
  (if (null? ls)
      nv
      (op (car ls)
          (foldr op nv (cdr ls)))))

(define (foldl op result ls)
  (if (null? ls)
      result
      (foldl op (op result (car ls)) (cdr ls))))