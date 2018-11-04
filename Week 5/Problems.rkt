#lang racket

(require rackunit)

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

(define-test-suite testMember
  (test-case "member with equal?"
             (check-equal? (member 1 (range 1 10))
                           (range 1 10))
             (check-equal? (member 9 (range 1 100))
                           (range 9 100))
             (check-equal? (member '(a . b) '(a a (a . b)))
                           '((a . b)))
             (check-false (member 9 (range 12 22)))))

(define (append lhs rhs)
  (foldRight cons rhs lhs))

(define (snoc u v) (cons v u))
(define (reverse list) (foldLeft snoc '() list))

(define (sorted? list)
  (define (for list previous)
    (if (null? list)
        #t
        (let ([current (car list)])
          (and (>= current previous)
               (for (cdr list) current)))))
  (or (null? list)
      (for (cdr list) (car list))))

(define-test-suite testSorted
  (test-case "sorted lists"
             (check-true (sorted? (range 1 1000)))
             (check-true (sorted? '(1 1 2 2 3 4 5))))
  (test-case "not sorted lists"
             (check-false (sorted? '(1 2 2 1 2 3 4 5)))
             (check-false (sorted? '(5 4 3 2 1))))
)

(define (collect-uniques1 list)
  (reverse
    (foldLeft
     (lambda (res current)
        (if (member current res)
            res
            (cons current res)))
     '()
     list)))

(define (collect-uniques list)
    (foldRight
     (lambda (current res)
        (if (member current res)
            res
            (cons current res)))
     '()
     list))

(define-test-suite testCollectUniques
  (test-case "non duplicates"
             (check-equal? (collect-uniques (range 1 100))
                           (range 1 100))
             (check-equal? (collect-uniques '("a" "b" "c"))
                           '("a" "b" "c")))
  (test-case "with duplicate"
             (check-equal? (collect-uniques '(1 1 1 2 2 3 4 5 6 7 8 9 9 10))
                           (range 1 10))
             (check-equal? (collect-uniques '('a 'a 'b 'c 'd 'd 'd 'd))
                           '('a 'b 'c 'd))))

(define (collect-ints list)
  (myFilter integer? list))

(define (insert x ls)
 (cond
   [(null? ls) (list x)]
   [(>= x (car ls)) (cons (car ls) (insert x (cdr ls)))]
   [else (cons x ls)]
 ))

(define-test-suite testInsert
  (test-case "()"
             (check-equal? (insert 4 '()) '(4)))
  (test-case "At the beginning"
             (check-equal? (insert 5 (range 6 10))
                           (range 5 10)))
  (test-case "At the end"
             (check-equal? (insert 10 (range 1 9))
                           (range 1 10)))
  (test-case "At the middle"
             (check-equal? (insert 4 '(1 2 3 5 6 7))
                           (range 1 7))))

(define (insertion-sort list)
  (foldLeft
   (lambda (sorted current)
     (insert current sorted))
   '()
   list))

(module+ test
  (require rackunit/text-ui)
  (run-tests testSorted)
  (run-tests testCollectUniques)
  (run-tests testMember)
  (run-tests testInsert))