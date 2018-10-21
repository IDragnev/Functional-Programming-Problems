#lang racket/base
+

(require rackunit)

(define (even? x) (= (modulo x 2) 0))
(define (// x y) (quotient x y))

(define (fast-pow x n)
  (define (square x) (* x x))
  (cond
    [(= n 0) 1]
    [(< n 0) (/ 1 (fast-pow x (- n)))]
    [(even? n) (square (fast-pow x (/ n 2)))]
    [else (* x (fast-pow x (- n 1)))]
    ))

(module+ test
 (test-case "Zero on any power is zero"
 (let ([pows (list 1 2 3 4 5 6 7 8)])
   (for-each
    (lambda (pow)
      (check-eq? (fast-pow 0 pow) 0))
    pows)))
  
  (test-case "n^1 is n"
       (let ([nums (list 1 2 3 4 5 6 7 8)])
   (for-each
    (lambda (n)
      (check-eq? (fast-pow n 1) n))
    nums)))
  
  (test-case "simple case"
     (check-eq? (fast-pow 2 3) 8))
)

(define (square x) (* x x))

(define (roots a b c)
  (define (discriminant a b c) (- (square b) (* 4 a c)))
  ( let ([D (discriminant a b c)])
     (cond
       [(and (= a 0) (= b 0) (= c 0)) 'inf]
       [(and (= a 0) (= b 0)) 0]
       [(= a 0) 1]
       [(= D 0) 1]
       [(> D 0) 2]
       [else 0]
   )))

(module+ test
    (test-case "D < 0 means no roots"
               (check-equal? (roots 1 1 1) 0))
    (test-case "D = 0 means one root"
               (check-equal? (roots 1 2 1) 1))
    (test-case "D > 0 means two roots"
               (check-equal? (roots 2 5 1) 2))
    (test-case "Linear equation has one root"
               (check-equal? (roots 0 5 1) 1))
    (test-case "(0,0,0) has infinite number of roots"
               (check-equal? (roots 0 0 0) 'inf))
    (test-case "(0, 0, c) has zero roots"
               (check-equal? (roots 0 0 1) 0))
  )

(define (factorial n)
  (define  (for i result)
      (if (> i n)
          result
          (for (+ i 1) (* result i))
          ))
  (for 2 1))
    

(module+ test
  (test-case "0! = 1"
             (check-equal? (factorial 0) 1))
  (test-case "5! = 120"
             (check-equal? (factorial 5) 120))
  )


(define (fib n)
  (define (for i prev curr)
    (if (= i n)
         prev
        (for (+ i 1) curr (+ prev curr))
     ))
  (for 0 0 1))

(module+ test
  (test-case "F(0)"
             (check-equal? (fib 0) 0))
  (test-case "F(8)"
             (check-equal? (fib 8) 21))
  )


(define % modulo)

(define (reverse-int num) 
  (define (helper num result)
     (if (= num 0)
          result
         (let* (
                [last (% num 10)]
                [result (+ (* result 10) last)]
                )
            (helper (// num 10) result)
       )))
   (helper num 0))
                               
                               
(module+ test
  (test-case "The reverse of 0 is 0"
             (check-eq? (reverse-int 0) 0))
  (test-case "Can reverse a big number"
             (check-eq? (reverse-int 12345) 54321))
  (test-case "Can reverse a small number"
             (check-eq? (reverse-int 1) 1))
  )

(define (palindrome? x) (= x (reverse-int x)))

(module+ test
  (test-case "0 is a palindrome"
             (check-true (palindrome? 0)))
  (test-case "Can detect a non-palindrome"
             (check-false (palindrome? 122)))
  (test-case "Long palindrome"
             (check-true (palindrome? 1234321)))
  )

(define (divisor? d num) (= (% num d) 0))
(define (sum-divisors num)
  (define (for i result)
           (if (>= i num)
                result
                (for
                   (+ i 1)
                   (if (divisor? i num)
                        (+ i result)
                        result
     ))))
  (for 1 num))

(module+ test
  (test-case "Nothing divides zero"
             (check-eq? (sum-divisors 0) 0))
  (test-case "Dividors of a prime number are just it and one"
             (check-eq? (sum-divisors 19) 20))
  (test-case "Dividors of a non-prime number are many"
             (check-eq? (sum-divisors 4) 7))
  )


(define (perfect? num) (= (- (sum-divisors num) num) num))

(module+ test
  (test-case "Zero is perfect"
             (check-true (perfect? 0)))
  (test-case "Recognizes perfect number"
             (check-true (perfect? 33550336)))
  (test-case "Recognizes non-perfect number"
             (check-false (perfect? 4)))
  )


(define (prime? num)
  (define stop (sqrt num))
  (define (for i)
            (if (>= i stop)
                 #t
                (and (not (divisor? i num))
                     (for (+ i 1))
                )))
  (cond
    [(= num 1) #f]
    [else (for 2)]
   ))

(module+ test
  (test-case "One is not prime"
             (check-false (prime? 1)))
  (test-case "Two is prime"
             (check-true (prime? 2)))
  (test-case "Simple prime"
             (check-true (prime? 107)))
  (test-case "Simple non-prime"
             (check-false (prime? 204)))
  )

(define (increasing? num)
  (define (for num digit)
    (if (= 0 num)
         #t
         (let ([last (% num 10)])
           (and
            (< last digit)
            (for (// num 10) last)
   ))))
  (for num 10))

(module+ test
  (test-case "One-digit number is increasing"
             (check-true (increasing? 0)))
  (test-case "Simple increasing"
             (check-true (increasing? 1234569)))
  (test-case "Simple non-increasing"
             (check-false (increasing? 54213)))
  )

(define (numberSystemNtoK num N K)
  (define (for num factor result)
            (if (= num 0)
                result
                (let (
                     [digit (% num K)]
                     [new-factor (* N factor)]
                     )
                  (for
                    (// num K)
                     new-factor
                    (+ result (* digit factor))
             ))))
  (for num 1 0))

(define (toBinary num) (numberSystemNtoK num 10 2))

(module+ test
  (test-case "0 in binary is 0"
             (check-eq? (toBinary 0) 0))
  (test-case "Big number to binary"
             (check-eq? (toBinary 294) 100100110))
  )

(define (toDecimal num) (numberSystemNtoK num 2 10))

(module+ test
  (test-case "0 in decimal is 0"
             (check-eq? (toDecimal 0) 0))
  (test-case "Big number to decimal"
             (check-eq? (toDecimal  100100110) 294))
  )