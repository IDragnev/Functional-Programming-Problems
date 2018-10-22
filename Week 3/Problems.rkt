#lang racket

(require rackunit)
(require math)

(define (accumulate acc proc begin end advance mapF stopPred)
  (if (stopPred begin end)
       acc
      (accumulate (proc acc (mapF begin)) proc (advance begin) end advance mapF stopPred)))

;;(define (accumulate-i acc proc begin end advance mapF stopPred)
  ;;(if (stopPred begin end)
    ;;   acc
      ;;(accumulate-i (proc (mapF begin) acc) proc (advance begin) end advance mapF stopPred)))

(define (!! n)
  (accumulate
   1
   *
   (if (even? n) 2 1)
   n
   (lambda (x) (+ x 2))
   (lambda (x) x)
   (lambda (begin end) (> begin end))
   ))


(module+ test
  (test-case "5!!"
             (check-equal? (!! 5) 15))
  (test-case "10!!"
             (check-equal? (!! 10) 3840)))

(define (nchk n k)
  (accumulate
   1
   *
   1
   k
   (lambda (k) (+ k 1))
   (lambda (k) (/ (- n (- k 1)) k))
   (lambda (begin end) (> begin end)))
   )

(module+ test
  (test-case "n choose 0 is 1"
             (check-equal? (nchk 10 0) 1))
  (test-case "n choose 1 is n"
             (check-equal? (nchk 100 1) 100))
  (test-case "55 choose 5 is 3478761"
             (check-equal? (nchk 55 5) 3478761))
  )

(define (2^ n)
  (accumulate
   0
   +
   0
   n
   (lambda (k) (+ k 1))
   (lambda (k) (nchk n k))
   (lambda (begin end) (> begin end)))
  )

(module+ test
  (test-case "2^0 = 1"
             (check-equal? (2^ 0) 1))
  (test-case "2^1 = 2"
             (check-equal? (2^ 1) 2))
  (test-case "2^10 = 1024"
             (check-equal? (2^ 10) 1024))
  (test-case "2^12 = 4096"
             (check-equal? (2^ 12) 4096))
   )


(define (divisors-sum n)
  (define (divisor? k) (= (modulo n k) 0))
  (accumulate
   0
   +
   1
   n
   (lambda (k) (+ k 1))
   (lambda (k) (if (divisor? k) k 0))
   (lambda (begin end) (> begin end)))
  )

(module+ test
  (test-case "if n is prime -> n + 1"
             (check-equal? (divisors-sum 107) 108))
  (test-case "Large non-prime"
             (check-equal? (divisors-sum 33550336) 67100672))
  )


(define (count p? a b)
  (accumulate
   0
   +
   a
   b
   (lambda (k) (+ k 1))
   (lambda (k) (if (p? k) 1 0))
   (lambda (begin end) (> begin end)))
  )


(module+ test
  (test-case
   "The number of even numbers in [2 ; 100] is 50"
   (check-equal? (count even? 2 100) 50))
  (test-case
   "1 has no divisors in [2 ; n], n >= 2"
   (check-equal?
    (count (lambda (x) (= (modulo 1 x) 0)) 2 10)
     0
    ))
  )


(define (all p? a b)  (= (count p? a b) (+ (- b a) 1)))
(define (any p? a b)  (> (count p? a b) 0))

(module+ test
  (test-case "all integers in [2 ; 10] are greater than 1"
             (check-true (all (lambda (x) (> x 1)) 2 10)))
  (test-case "no integer in [2 ; 10] is greater than 100"
             (check-false (all (lambda (x) (> x 100)) 2 10)))
  (test-case "[64 ; 110] contains a prime number"
             (check-true (any prime? 64 110)))
  (test-case "no integer in [2; 10] divides 1"
             (check-false (any (lambda (x) (= (modulo 1 x) 0)) 2 10)))
  )


(define (prime? n)
  (define (divisor? k) (= (modulo n k) 0))
  (and
   (not (= n 1))
   (not (any (lambda (x) (divisor? x)) 2 (sqrt n)))
  )
)

(module+ test
  (test-case "1 is not a prime"
             (check-false (prime? 1)))
  (test-case "2 is a prime"
             (check-true (prime? 2)))
  (test-case "107 is a prime"
             (check-true (prime? 107)))
   )


                                  
