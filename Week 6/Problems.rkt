#lang racket

(define (foldl op result list)
  (if (null? list)
      result
      (foldl op (op result (car list)) (cdr list))))

(define (del-assoc key assocList)
  (filter (lambda (pair) (not (equal? (car pair) key))) assocList))

(define keyOf car)

(define valueOf cdr)

(define (group-by f lst) 
  (define (combineValues lhs rhs) (append (valueOf lhs) (valueOf rhs)))
  (define (merge lhs rhs) (cons (keyOf lhs) (combineValues lhs rhs)))
  (let ([mapped (map (lambda (x) (cons (f x) (list x))) lst)])
   (foldl
     (lambda (result pair)
       (let ([keyGroup (assoc (keyOf pair) result)])
          (if (not keyGroup)
              (cons pair result)
              (cons (merge pair keyGroup)
                    (del-assoc (keyOf keyGroup) result)))))            
     '()
     mapped)))
  
(define (prefix? lhs rhs)
  (or (null? lhs)
      (and (not (null? rhs))
           (equal? (car lhs) (car rhs))
           (prefix? (cdr lhs) (cdr rhs)))))
 
(define (infix? lhs rhs)
  (or (prefix? lhs rhs)
      (infix? lhs (cdr rhs))))
 
(define (remove-duplicates lst)
  (define (for lst result)
    (cond
      [(null? lst) result]
      [(member (car lst) result) (for (cdr lst) result)]
      [else (for (cdr lst) (cons (car lst) result))]))
  (for lst '()))
 
(define (++ n) (+ n 1))
 
(define (count-occurances x list)
  (foldl
   (lambda (result y)
     (if (equal? x y) (++ result) result))
   0
   list))