#lang racket

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


;; task 1
(define // quotient)
(define % modulo)
(define (++ n) (+ n 1))
(define (-- n) (- n 1))

(define (reverse num)
  (define (for num result)
    (if (= 0 num)
        result
        (for
          (// num 10)
          (+ (* 10 result) (% num 10)))))
  (for num 0))

(define (diffReverse num)
  (- num (reverse num)))

(define (timesFound digit num)
  (define (for num result)
    (cond
      [(< num 10) (if (= digit num) (++ result) result)]
      [(= (% num 10) digit) (for (// num 10) (++ result))]
      [else (for (// num 10)  result)]))
  (for num 0))

(define (insertTimes n digit num)
    (if (= n 0)
        num
        (insertTimes (-- n) digit (+ (* num 10) digit))))

(define (sortDigits num)
  (define (for i result)
    (if (< i 0)
        result
        (let ([times (timesFound i num)])
          (for (-- i) (insertTimes times i result)))))
  (for 9 0))

;; task 2

(define (even? num) (= (% num 2) 0))
(define (id x) x)

(define (range start end)
  (define (for i result)
    (if (< i start)
        result
        (for (-- i) (cons i result))))
  (for end '()))

(define (compose f g) (lambda (x) (f (g x))))
(define (exch-compose f g times)
  (accumulate
   compose id
   1 times
   (lambda (i) (if (even? i) g f))
   ++))

(define (property? x f g)
  (let ([composOne (exch-compose f g x)]
        [composTwo (exch-compose g f x)])
    (equal? (composOne x) (composTwo x))))

(define (permutable? a b f g)
  (let ([evens (filter even? (range a b))])
    (foldl
     (lambda (result current)
       (and result (property? current f g)))
     #t
     evens)))

;; task 3

(define (intervalLen interval)
  (++ (- (cdr interval) (car interval))))

(define (longer? lhs rhs)
  (> (intervalLen lhs) (intervalLen rhs)))

(define (findLongest intervals)
  (foldl
   (lambda (longest current)
     (if (longer? current longest)
         current
         longest))
   (car intervals)
   (cdr intervals)))

(define (subset? interval target)
  (and (>= (car interval) (car target))
       (<= (cdr interval) (cdr target))))

(define (insert pair lst)
  (define (for lst result)
    (cond
      [(null? lst) (append result (list pair))]
      [(>= (caar lst) (car pair)) (append result (cons pair lst))]
      [else (for (cdr lst) (append result (list (car lst))))]))
  (for lst '()))

(define (sort list)
  (foldl
   (lambda (result current)
     (insert current result))
   '()
   list))

(define (longest-interval-subsets intervals)
  (if (null? intervals)
        '()
        (let ([longest (findLongest intervals)])
          (sort
           (filter
            (lambda (interval) (subset? interval longest))
            intervals)))))
   