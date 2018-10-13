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








