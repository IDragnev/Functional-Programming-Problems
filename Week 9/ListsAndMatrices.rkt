#lang racket

(define (prefix? lhs rhs)
  (or (null? lhs)
      (and (not (null? rhs))
           (equal? (car lhs) (car rhs))
           (prefix? (cdr lhs) (cdr rhs)))))

(define (infix? lhs rhs)
  (or (prefix? lhs rhs)
      (and (not (null? rhs))
           (infix? lhs (cdr rhs)))))

(define (member x list)
  (cond
    [(null? list) #f]
    [(equal? x (car list)) list]
    [else (member x (cdr list))]))

(define (makeSet list)
  (if (null? list)
      '()
      (let ([head (car list)]
            [tail (cdr list)])
        (cond
          [(member head tail) (makeSet tail)]
          [else (cons head (makeSet tail))]))))

(define (search p list)
  (and (not (null? list))
       (or (p (car list))
           (search p (cdr list)))))

(define (-- n) (- n 1))

(define (range start end)
  (define (for i result)
    (if (< i start)
        result
        (for (-- i) (cons i result))))
  (for end '()))

(define (list-ref n list)
  (if (zero? n)
      (car list)
      (list-ref (-- n) (cdr list))))

(define (drop n list)
  (if (zero? n)
      list
      (drop (-- n) (cdr list))))

(define M '((1 2 3) (4 5 6) (7 8 9)))

(define (row i m) (list-ref i m))

(define (column i m) (map (lambda (row) (list-ref i row)) m))

(define numberOfRows length)
(define (numberOfCols m) (length (row 0 m)))

(define dropRows drop)

(define (upperTriangle m)
  (if (null? m)
      '()
      (cons (row 0 m)
            (upperTriangle
             (map cdr (dropRows 1 m))))))

(define (accumulate op result start end f next stop?)
  (if (stop? start end)
      result
      (accumulate op (op result (f start)) (next start) end f next stop?)))

(define (accumulate-r op nv start end f next stop?)
  (if (stop? start end)
      nv
      (op (f start) (accumulate-r op nv (next start) end f next stop?))))

(define (++ n) (+ n 1))

(define (transpose m)
  (accumulate-r cons '()
              0 (-- (numberOfCols m))
              (lambda (i) (column i m))
              ++ >))

(define (foldl op result list)
  (if (null? list)
      result
      (foldl op (op result (car list)) (cdr list))))

(define (descartes lhs rhs)
  (foldl
   (lambda (result x)
     (let ([pairs
           (foldl
            (lambda (result y)
              (cons (cons x y) result))
            '()
            rhs)])
       (cons pairs result)))   
   '()
   lhs))

(define (sumVectors u v) (map + u v))
(define (multiplyVectors u v) (apply + (map * u v)))

(define (multiplyMatrices lhs rhs)
  (let ([transposedRhs (transpose rhs)])
    (map
     (lambda (row)
       (map
        (lambda (column)
          (multiplyVectors row column))
        transposedRhs))
     lhs)))
