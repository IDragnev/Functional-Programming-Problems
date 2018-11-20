#lang racket

(define the-empty-stream '())

(define empty-stream? null?)

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t)
                    (cons h (delay t)))))

(define head car)

(define (tail stream)
  (force (cdr stream)))

(define (enum a b)
  (cons-stream a (enum (+ a 1) b)))

(define (first n stream)
  (if (or (empty-stream? stream) (zero? n))
      '()
      (cons (head stream) (first (- n 1) (tail stream)))))

(define (search-stream p? s)
  (cond
    [(empty-stream? s) #f]
    [(p? (head s)) s]
    [else (search-stream p? (tail s))]
    ))

(define (from n)
  (cons-stream n (from (+ n 1))))

;;(define nats (from 0))

;;(define (generate-fibs a b)
;;  (cons-stream a (generate-fibs b (+ a b))))

;;(define fibs (generate-fibs 0 1))

(define (filter-stream p? s)
  (if (p? (head s))
      (cons-stream (head s) (filter-stream p? (tail s)))
      (filter-stream p? (tail s))))

(define (map-stream f . streams)
  (cons-stream (apply f (map head streams))
               (apply map-stream f (map tail streams))))

(define ones (cons-stream 1 ones))

(define (++ n) (+ n 1))

(define nats (cons-stream 0 (map-stream ++ nats)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (map-stream + fibs (tail fibs)))))

(define (notdivides d) (lambda (n) (> (remainder n d) 0)))

(define (sieve stream)
  (cons-stream (head stream)
               (sieve (filter-stream
                       (notdivides (head stream))
                       (tail stream)))))

(define primes (sieve (from 2)))

