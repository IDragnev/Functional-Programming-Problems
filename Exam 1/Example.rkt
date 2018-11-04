#lang racket

(require "Functions.rkt")

(define (++ n) (+ n 1))
(define (-- n) (- n 1))
(define // quotient)
(define % modulo)
(define (even? num) (zero? (% num 2)))

; task 1

(define (length num)
  (define (for num result)
    (if (< num 10)
        (++ result)
        (for (// num 10) (++ result))))
  (for num 0))

(define (nth-digit-tail n num)
  (if (zero? n)
      (% num 10)
      (nth-digit-tail (-- n) (// num 10))))

(define (middle-digit num)
  (let ([len (length num)])
    (if (even? len)
        -1
        (nth-digit-tail (// len 2) num))))

; task 2
(define (member? x list)
  (cond
    [(null? list) #f]
    [(equal? x (car list)) #t]
    [else (member? x (cdr list))]
 ))

(define (mapsToSameList? f list)
  (foldl
   (lambda (result current)
     (and result (member? (f current) list)))
   #t
   list))

(define (preservesOp? list op f)
  (define (helper x list op f)
    (foldl
     (lambda (result y)
       (and result
            (equal? (op (f x) (f y))
                    (f (op x y)))))
     #t
     list))
  (foldl
   (lambda (result x)
     (and result (helper x list f op)))
   #t
   list))

(define (is-em? list op f)
  (and (mapsToSameList? f list)
       (preservesOp? list op f)))

; task 3
(define (meetTwice? f g start end)
  (>=
   (accumulate
    + 0
    start end
    (lambda (x) (if (equal? (f x) (g x)) 1 0))
    ++)
   2))

; task 4
(define (append destination source)
  (foldr cons source destination))

(define (next-look-and-say lst)
  (define (for current times result lst)
    (if (null? lst)
        (append result (list times current))
        (let ([head (car lst)]
              [tail (cdr lst)])
          (if (equal? head current)
              (for current (++ times) result tail)
              (for head 1 (append result (list times current)) tail)))))
  (for (car lst) 1 '() (cdr lst)))



   
