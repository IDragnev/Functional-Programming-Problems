#lang racket

(define (foldr op nv list)
  (if (null? list)
      nv
      (op (car list) (foldr op nv (cdr list)))))

(define (foldl op result list)
  (if (null? list)
      result
      (foldl op (op result (car list)) (cdr list))))

(define (atom? x) (and (not (null? x)) (not (pair? x))))

(define (count-atoms list)
  (deep-foldr + 0 (lambda (x) 1) list))

(define (flatten lst)
  (deep-foldr append '() list lst))

(define (rcons x lst) (append lst (list x)))

(define (id x) x)

(define (deep-reverse lst)
  (deep-foldr rcons '() id lst))

(define (branch p? f g) (lambda (x) (if (p? x) (f x) (g x))))

(define (deep-foldr op nv f lst)
  (foldr op nv
         (map
          (branch atom? f (lambda (ls) (deep-foldr op nv f ls)))
          lst)))

(define (deep-foldl op result f lst)
  (foldl op result
         (map
          (branch atom? f (lambda (ls) (deep-foldl op result f ls)))
          lst)))

(define (count-atoms2 ls)
  (deep-foldl + 0 (lambda (x) 1) ls))

(define (flatten2 ls)
  (deep-foldl append '() list ls))

(define (snoc ls x) (cons x ls))

(define (deep-reverse2 ls)
  (deep-foldl snoc '() id ls))

(define (append . args)
    (cond
      [(null? args) '()]
      [(null? (car args)) (apply append (cdr args))]
      [else (cons (caar args)
                  (apply append (cons (cdar args) (cdr args))))]
))

(define (maximum x . args)
  (foldl max x args))
