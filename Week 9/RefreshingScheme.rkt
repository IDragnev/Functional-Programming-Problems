#lang racket

(define (foldl op result list)
  (if (null? list)
      result
      (foldl op (op result (car list)) (cdr list))))

(define (duplicate list)
  (reverse
   (foldl
    (lambda
        (result x) (cons x (cons x result)))
    '()
    list)))

(define (++ n) (+ n 1))

(define (maximum list)
  (and (not (null? list))
       (foldl
        (lambda (max x) (if (> x max) x max))
        (car list)
        (cdr list))))

(define (maxRepeated list)
  (define (for lens count last list)
    (cond
      [(null? list) (cons count lens)]
      [(equal? (car list) last) (for lens (++ count) last (cdr list))]
      [else (for (cons count lens) 1 (car list) (cdr list))]))
  (if (null? list)
      0
      (let ([lens (for '() 1 (car list) (cdr list))])
        (maximum lens))))

(define (accumulate op result start end next term stop?)
  (if (stop? start end)
      result
      (accumulate op (op result (term start)) (next start) end next term stop?)))