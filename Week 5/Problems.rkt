#lang racket

(define (foldRight op nv ls)
  (if (null? ls)
      nv
      (op (car ls)
          (foldRight op nv (cdr ls)))))

(define (foldLeft op result ls)
  (if (null? ls)
      result
      (foldLeft op (op result (car ls)) (cdr ls))))

(define (myMap f list)
  (foldRight
   (lambda (current nv) (cons (f current) nv))
   '()
   list))

(define (myFilter p? list)
  (foldRight
   (lambda (current nv)
     (if (p? current)
         (cons current nv)
         nv))
   '()
   list))

(define (foldRightNonEmpty op ls)
  (if (null? (cdr ls))
      (car ls)
      (op (car ls) (foldRightNonEmpty op (cdr ls)))))

(define (foldLeftNonEmpty op ls)
  (foldLeft op (car ls) (cdr ls)))

(define (atom? x) (and (not (null? x)) (not (pair? x))))
(define (countAtoms list)
  (cond
    [(null? list) 0]
    [(atom? list) 1]
    [else (+ (countAtoms (car list)) (countAtoms (cdr list)))]
   ))

(define (++ n) (+ n 1))
(define (-- n) (- n 1))
(define % modulo)
(define // quotient)

(define (range from to)
  (define (for i result)
    (if (< i from)
        result
        (for (-- i) (cons i result))))
  (for to '()))

(define (digit-list num)
  (define (for num result)
    (if (zero? num)
        result
        (for (// num 10) (cons (% num 10) result))))
  (for num '()))

(define (take n list)
  (define (for n list result)
    (if (zero? n)
        (reverse result)
        (for (-- n) (cdr list) (cons (car list) result))))
  (for n list '()))

(define (drop n list)
  (if (zero? n)
      list
      (drop (-- n) (cdr list))))

(define (n-th n list)
  (car (drop n list)))

(define (all? p? list)
  (foldLeft
   (lambda (lhs rhs) (and lhs rhs))
   #t
   (myMap p? list)))

(define (any? p? list)
  (not (all? (lambda (x) (not (p? x))) list)))

(define (zip f lhs rhs)
  (define (for lhs rhs result)
    (if (or (null? lhs)
            (null? rhs))
         (reverse result)
         (for (cdr lhs)
              (cdr rhs)
              (cons
               (f (car lhs) (car rhs))
               result))))
  (for lhs rhs '()))

(define (mem p? x list)
  (cond
    [(null? list) #f]
    [(p? x (car list)) list]
    [else (mem p? x (cdr list))]
  ))

(define (member x list) (mem equal? x list))
(define (memv x list) (mem eqv? x list))
(define (memq x list) (mem eq? x list))

(define (append lhs rhs)
  (foldRight cons rhs lhs))

(define (snoc u v) (cons v u))
(define (reverse list) (foldLeft snoc '() list))
