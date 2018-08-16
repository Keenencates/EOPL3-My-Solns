#lang racket
;ex9

(define (remove s lst)
  (cond
    [(or (null? lst)(empty? lst)) '()]
    [(eqv? s (first lst))(remove s (rest lst))]
    [else (cons (first lst) (remove s (rest lst)))]))

(remove 'x '(a x b x c x d x z))