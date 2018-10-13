#lang racket/base
+

(require rackunit)

(define even? (lambda (x) (= (modulo x 2) 0)))
(define // (lambda (x y) (quotient x y)))
(define fast-pow (lambda (num pow)
                   (cond
                     [(= pow 0) 1]
                     [(even? pow)
                      (let ([x (fast-pow num (// pow 2))])
                                    (* x x))]
                     [else (* num (fast-pow num (- pow 1)))]
                     )))

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

(define square (lambda (x) (* x x)))
(define discriminant (lambda (a b c)
                       (- (square b) (* 4 a c))))
(define roots (lambda (a b c)
                ( let ([D (discriminant a b c)])
                   (cond
                     [(= D 0) 1]
                     [(> D 0) 2]
                     [else 0]
                     ))))

(module+ test
    (test-case "D < 0 means no roots"
               (check-equal? (roots 1 1 1) 0))
    (test-case "D = 0 means one root"
               (check-equal? (roots 1 2 1) 1))
    (test-case "D > 0 means two roots"
               (check-equal? (roots 2 5 1) 2))
  )

(define do-factorial (lambda (n result)
                       (if (= n 0)
                           result
                           (do-factorial (- n 1) (* result n))
                            )))
(define factorial (lambda (n) (do-factorial n 1)))

(module+ test
  (test-case "0! = 1"
             (check-equal? (factorial 0) 1))
  (test-case "5! = 120"
             (check-equal? (factorial 5) 120))
  )

(define n-fib (lambda (n a b)
                (case n
                  [(0) a]
                  [(1) b]
                  [else (n-fib (- n 1) b (+ a b))]
                   )))
(define fib (lambda (n) (n-fib n 0 1)))


(module+ test
  (test-case "The first fibonacci num is 0"
             (check-equal? (fib 0) 0))
  (test-case "The 8th fibonacci num is 21"
             (check-equal? (fib 8) 21))
  )


(define % modulo)
(define do-reverse-int (lambda (num result)
                         (if (= num 0)
                             result
                             (let* (
                                    [last (% num 10)]
                                    [result (+ (* result 10) last)]
                                    )
                               (do-reverse-int (// num 10) result)
                               ))))
(define reverse-int (lambda (num) (do-reverse-int num 0)))
                               
                               
(module+ test
  (test-case "The reverse of 0 is 0"
             (check-eq? (reverse-int 0) 0))
  (test-case "Can reverse a big number"
             (check-eq? (reverse-int 12345) 54321))
  (test-case "Can reverse a small number"
             (check-eq? (reverse-int 1) 1))
  )

(define palindrome? (lambda (x) (= x (reverse-int x))))

(module+ test
  (test-case "0 is a palindrome"
             (check-true (palindrome? 0)))
  (test-case "Can detect a non-palindrome"
             (check-false (palindrome? 122)))
  (test-case "Long palindrome"
             (check-true (palindrome? 1234321)))
  )


(define divisor? (lambda (d num) (= (% num d) 0)))
(define do-sum-divisors (lambda (num d result)
                           (if (= d 0)
                               result
                               (do-sum-divisors
                                  num
                                  (- d 1)
                                  (if (divisor? d num)
                                     (+ d result)
                                     result
                                   )
                            ))))
(define sum-divisors (lambda (num) (do-sum-divisors num num 0)))

(module+ test
  (test-case "Nothing divides zero"
             (check-eq? (sum-divisors 0) 0))
  (test-case "Dividors of a prime number are just it and one"
             (check-eq? (sum-divisors 19) 20))
  (test-case "Dividors of a non-prime number are many"
             (check-eq? (sum-divisors 4) 7))
  )


(define perfect? (lambda (num) (= (- (sum-divisors num) num) num)))

(module+ test
  (test-case "Zero is perfect"
             (check-true (perfect? 0)))
  (test-case "Recognizes perfect number"
             (check-true (perfect? 33550336)))
  (test-case "Recognizes non-perfect number"
             (check-false (perfect? 4)))
  )


(define has-divisor-in-range? (lambda (num start end)
                                 (if (> start end)
                                     #f
                                     (or (divisor? start num)
                                          (has-divisor-in-range? num (+ start 1) end)
                                     )
                                   )))
(define prime? (lambda (num) (and (not (= 1 num)) (not (has-divisor-in-range? num 2 (- num 1))))))

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

(define check-increasing (lambda (num last-checked)
                           (or (= num 0)
                               ( let ([last-digit (% num 10)])
                                  (and
                                   (< last-digit last-checked)
                                   (check-increasing (// num 10) last-digit)
                                   )))))
(define increasing? (lambda (num) (check-increasing num (+ num 1))))

(module+ test
  (test-case "One-digit number is increasing"
             (check-true (increasing? 0)))
  (test-case "Simple increasing"
             (check-true (increasing? 1234569)))
  (test-case "Simple non-increasing"
             (check-false (increasing? 54213)))
  )

(define transform (lambda (num factor result)
                    (if (= num 0)
                        result
                        (let (
                              [digit (% num 2)]
                              [new-factor (* 10 factor)]
                              )
                          (transform
                            (// num 2)
                             new-factor
                             (+ result (* digit factor))
                                )))))
(define toBinary (lambda (num)
                   (transform num 1 0)))

(module+ test
  (test-case "0 in binary is 0"
             (check-eq? (toBinary 0) 0))
  (test-case "Big number to binary"
             (check-eq? (toBinary 294) 100100110))
  )

(define doToDecimal (lambda (num power result)
                      (if (= num 0)
                          result
                          ( let* (
                                 [LSB (% num 10)]
                                 [product (* LSB (fast-pow 2 power))]
                                 [result (+ result product)]
                                 )
                             (doToDecimal
                               (// num 10)
                               (+ power 1)
                               result)))))
                      
(define toDecimal (lambda (num) (doToDecimal num 0 0)))

(module+ test
  (test-case "0 in decimal is 0"
             (check-eq? (toDecimal 0) 0))
  (test-case "Big number to decimal"
             (check-eq? (toDecimal  100100110) 294))
  )