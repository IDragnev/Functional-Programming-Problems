#lang racket

(require rackunit)
(require math)

(define (++ x) (+ x 1))
(define (-- x) (- x 1))
(define % modulo)
(define (id x) x)

(define (any? begin end p? next stop?)
  (and (not (stop? begin end))
       (or (p? begin)
           (any? (next begin) end p? next stop?))
 ))

(define (all? begin end p? next stop?)
  (not (any? begin end (lambda (x) (not (p? x))) next stop?))
 )
      
(define (prime? num)
  (define (divisor? k) (zero? (% num k)))
  (and (not (= num 1))
       (not (any? 2 (sqrt num) divisor? ++ >))))

(define-test-suite testPrime
  (test-case "false for non-primes"
             (check-false (prime? 1))
             (check-false (prime? 4))
             (check-false (prime? 202))
             (check-false (prime? 1000)))
  (test-case "true for primes"
             (check-true (prime? 2))
             (check-true (prime? 107))
             (check-true (prime? 10007)))
)

(define-test-suite testAnyAndAll
  (test-case "[0 ; 100] contains no negative numbers"
             (check-false (any? 0 100 (lambda (x) (< x 0)) ++ >)))
  (test-case "[3 ; 9999999] contains a prime number"
             (check-true (any? 6 9999999 prime? ++ >)))
  (test-case "[1 ; 100] contains only positive numbers"
             (check-true (all? 1 100 positive? ++ >)))
  (test-case "[0; 1000] contains non-positive number"
             (check-false (all? 0 1000 positive? ++ >)))
)

(define (length ls)
  (define (for ls n)
    (if (null? ls)
        n
        (for (cdr ls) (++ n))))
  (for ls 0))

(define-test-suite testLenght
  (test-case "() has length 0"
             (check-equal? (length '()) 0))
  (test-case "handles long list"
             (check-equal? (length (range 1 99999999)) 99999999))
  )

(define (reverse ls)
  (define (for ls result)
    (if (null? ls)
        result
        (for (cdr ls)
             (cons (car ls) result))))
 (for ls '()))

(define-test-suite testReverse
  (test-case "()"
             (check-equal? (reverse '()) '()))
  (test-case "Reverses a large list"
             (check-equal? (reverse (range 1 10000)) (range 9999 0 -1)))
)

(define (map ls f)
  (define (for ls result)
    (if (null? ls)
        (reverse result)
        (for (cdr ls) (cons (f (car ls)) result))))
  (for ls '()))

(define-test-suite testMap
  (test-case "square a list"
             (check-equal?
              (map '(1 2 3 4) (lambda (x) (* x x)))
              '(1 4 9 16)))
  (test-case "identity on a long list"
             (check-equal?
              (map (range 1 999999) id)
              (range 1 999999)))
  )

(define (filter p? ls)
  (define (for ls result)
    (if (null? ls)
        (reverse result)
        (let ([head (car ls)])
          (for (cdr ls)
               (if (p? head)
                   (cons head result)
                   result)))))
 (for ls '()))

(define-test-suite testFilter
  (test-case "filter odds"
             (check-equal? (filter even? (range 0 1000))
                           (range 0 1000 2)))
  (test-case "filter primes"
             (check-equal? (filter prime? (range 1 20))
                           '(2 3 5 7 11 13 17 19)))
)

(define (nth n ls)
  (cond
      [(and (null? ls) (>= n 0)) #f]
      [(zero? n) (car ls)]
      [else (nth (-- n) (cdr ls))]
 ))

(define-test-suite testNth
  (test-case "First"
             (check-equal? (nth 0 '(1 2 3)) 1))
  (test-case "Fourth"
             (check-equal? (nth 4 '(1 2 3 4 5)) 5))
  (test-case "Out of range"
             (check-equal? (nth 4 '(1 2 3 4)) #f)
             (check-equal? (nth 100 '(1 2 3)) #f))
)

(define (zip f firstList secondList)
  (define (for firstList secondList result)
    (if (null? firstList)
         (reverse result)
         (for (cdr firstList)
              (cdr secondList)
              (cons (f (car firstList)
                       (car secondList))
                    result))))
  (for firstList secondList '()))

(define-test-suite testZip
  (test-case "Sum"
             (check-equal?
              (zip + (range 1 11) (range 1 11))
              (range 2 21 2)))
  (test-case "Square"
             (check-equal?
              (zip * (range 1 11) (range 1 11))
              (map (range 1 11) (lambda (x) (* x x)))))
)

(define (stop? begin end) (equal? begin end))
(define (applyToHead f) (lambda (ls) (f (car ls))))

(define-test-suite testAnyForLists
  (test-case "True"
             (check-true
              (any? (range 1 10) '() (applyToHead prime?) cdr stop?))
             (check-true
              (any? (range 1 100) '() (applyToHead even?) cdr stop?)))
  (test-case "False"
             (check-false (any? '() '() (applyToHead prime?) cdr stop?))
             (check-false
              (any? (range 4 100 2) '() (applyToHead prime?) cdr stop?))
             (check-false
              (any? (range 2 50) '() (applyToHead zero?) cdr stop?)))
  )

(define-test-suite testAllForLists
  (test-case "True"
             (check-true (all? '() '() (applyToHead prime?) cdr stop?))
             (check-true
              (all? '(3 5 7 11 13) '() (applyToHead prime?) cdr stop?))
             (check-true
              (all? (range 2 100 2) '() (applyToHead even?) cdr stop?)))
  (test-case "False"
             (check-false
              (all? '(2 3 5 7 10 11) '() (applyToHead prime?) cdr stop?))
             (check-false
              (all? '(0 0 0 1 0 0) '() (applyToHead zero?) cdr stop?)))
  )

(module+ test
  (require rackunit/text-ui)
  (run-tests testPrime)
  (run-tests testAnyAndAll)
  (run-tests testReverse)
  (run-tests testMap)
  (run-tests testFilter)
  (run-tests testNth)
  (run-tests testZip)
  (run-tests testAnyForLists)
  (run-tests testAllForLists)
  )