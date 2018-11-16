#lang racket

(define (makeHash f keys)
  (map  (lambda (key) (cons key (f key))) keys))

(define h (makeHash (lambda (x) (* x 2)) (range 1 10)))

(define (keys hash)
  (map car hash))

(define (values hash)
  (map cdr hash))

(define (del-assoc value hash)
  (filter (lambda (pair) (not (equal? (car pair) value))) hash))

(define (add-assoc key value hash)
  (cons (cons key value) (del-assoc key hash)))

(define (search p lst)
  (and (not (null? lst))
       (or (p (car lst))
           (search p (cdr lst)))))

(define (assc =? key hash)
  (search (lambda (pair) (and (=? (car pair) key) pair)) hash))

(define (assoc key hash)
  (assc equal? key hash))

(define (assqv key hash)
  (assc eqv? key hash))

(define (assq key hash)
  (assc eq? key hash))