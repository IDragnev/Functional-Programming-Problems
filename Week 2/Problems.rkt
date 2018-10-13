#lang racket
+
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


(define do-factorial (lambda (n result)
                       (if (= n 0)
                           result
                           (do-factorial (- n 1) (* result n))
                            )))
(define factorial (lambda (n) (do-factorial n 1)))


(define n-fib (lambda (n a b)
                (case n
                  [(0) a]
                  [(1) b]
                  [else (n-fib (- n 1) b (+ a b))]
                   )))
(define fib (lambda (n) (n-fib n 0 1)))
