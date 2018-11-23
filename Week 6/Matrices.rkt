#lang racket

(define m '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(define (-- n) (- n 1))
(define (++ n) (+ n 1))

(define (foldl op result list)
  (if (null? list)
      result
      (foldl op (op result (car list)) (cdr list))))

(define (accumulate op result start end f next stop?)
  (if (stop? start end)
      result
      (accumulate op (op result (f start)) (next start) end f next stop?)))

(define (accumulate-rec op nv start end f next stop?)
  (if (stop? start end)
      nv
      (op (f start) (accumulate-rec op nv (next start) end f next stop?))))

(define (all? p? list)
  (foldl (lambda (res current)
           (and res current))
         #t
         (map p? list)))

(define (matrix? m)
  (and (list? m)
       (not (null? m))
       (not (null? (car m)))
       (all? list? m)
       (let ([len (length (car m))])
         (all? (lambda (row) (= (length row) len)) m))))

(define (numberOfCols m) (length (car m)))

(define numberOfRows length)

(define (row i m) (list-ref m i))

(define (column i m)
  (map (lambda (row) (list-ref row i)) m))

(define (removeIth i list)
  (define (for i result list)
    (if (= i 0)
        (append (reverse result) (cdr list))
        (for (-- i)
          (cons (car list) result)
          (cdr list))))
  (for i '() list))

(define removeRow removeIth)

(define (removeColumn i m)
  (map (lambda (row) (removeIth i row)) m))

(define (transpose-rec m)
  (if (null? (row 0 m))
      '()
      (cons (column 0 m)
            (transpose-rec (removeColumn 0 m)))))

(define (transpose m)
  (accumulate-rec cons '()
              0 (-- (numberOfCols m))
              (lambda (i) (column i m))
              ++ >))

(define (sumVectors u v) (map + u v))
(define (multiplyVectors u v) (apply + (map * u v)))

(define (sumMatrices m1 m2) (map sumVectors m1 m2))

(define (multiplyMatrices m1 m2)
  (let ([m2t (transpose m2)])
    (map (lambda (row)
           (map (lambda (column) (multiplyVectors row column))
                m2t))
           m1)))

(define (scaleVector u n)
  (map (lambda (x) (* x n)) u))

(define (scaleMatrix m n)
  (map (lambda (row) (scaleVector row n)) m))

(define (countStartingZeros row)
  (define (for n row)
    (if (or (null? row)
            (not (zero? (car row))))
         n
      (for (++ n) (cdr row))))
  (for 0 row))

(define (triangular? m)
  (define (for i m)
    (or (null? m)
        (and
         (eqv? (countStartingZeros (car m)) i)
         (for (++ i) (cdr m)))))
    (and (eqv? (numberOfRows m)(numberOfCols m))
         (for 0 m)))

(define (rcons list x) (cons x list))

(define (mainDiag m)
  (accumulate
   rcons '()
   (-- (numberOfRows m)) 0
   (lambda (i) (list-ref (row i m) i))
   -- <))

(define (secondDiag m)
  (let ([cols (-- (numberOfCols m))])    
    (accumulate
     rcons '()
     (-- (numberOfRows m)) 0
     (lambda (i) (list-ref (row i m) (- cols i)))
     -- <)))

(define (descartes lhs rhs)
  (define (collectPairs x)
    (foldl
     (lambda (result y) (cons (cons x y) result))
     '()
     rhs))
  (foldl
   (lambda (result x)
     (append result (collectPairs x)))                 
   '()
   lhs))