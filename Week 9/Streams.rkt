#lang racket

(define (fact n) (* n (fact (- n 1))))

(define empty-stream '())
;поток: (h . t) където h е глава, t е обещание за поток : (1 . promise)
(define head car)

(define (tail stream) (force (cdr stream)))

(define empty-stream? null?)

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t) (cons h (delay t)))))

(define (enum from to)
  (if (> from to)
      empty-stream
      (cons-stream
       from
       (enum (+ from 1) to))))

(define (take n stream)
  (if (zero? n)
      '()
      (cons (head stream)
            (take (- n 1) (tail stream)))))

(define (search-stream p s)
  (cond
    [(empty-stream? s) #f]
    [(p (head s)) s]
    [else (search-stream p (tail s))]))

(define (from n)
  (cons-stream n (from (+ n 1))))

(define (filter-stream p s)
  (if (p (head s))
      (cons-stream (head s) (filter-stream p (tail s)))
      (filter-stream p (tail s))))

(define (zip-streams f lhs rhs)
  (cons-stream (f (head lhs) (head rhs))
               (zip-streams f (tail lhs) (tail rhs))))

(define (map-streams  f . args)
  (cons-stream
   (apply f (map head args))
   (apply map-streams f (map tail args))))

(define ones (cons-stream 1 ones))

(define nats
  (cons-stream 0
               (map-streams
                (lambda (x) (+ x 1))
                nats)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (map-streams + fibs (tail fibs)))))

(define (isNotDivisor d)
  (lambda (n)
    (> (remainder n d) 0)))

(define (sieve s)
  (cons-stream (head s)
               (sieve
                (filter-stream
                 (isNotDivisor (head s))
                 (tail s)))))

(define (repeat n)
  (cons-stream n (repeat n)))

(define (cycle list)
  (define (do-cycle remaining)
    (if (null? remaining)
        (do-cycle list)
        (cons-stream (head remaining)
                     (do-cycle (tail remaining)))))
  (do-cycle list))

(define (iterate f x)
  (let ([y (f x)])
    (cons-stream y
                 (iterate f y))))