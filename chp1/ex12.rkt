#lang racket
;ex12
(define (subst new old sexp)
  (cond
    [(null? sexp) '()]
    [(and (symbol? sexp)(eqv? sexp old)) new]
    [(symbol? sexp) sexp]
    [else (cons (subst new old (first sexp))
                (subst new old (rest sexp)))]))

(subst 'homer-simpson 'x '(x a b x a x))
(subst 'a 'x 'x)