#lang racket

;ex1.13

(define (subst new old sexp)
  (cond
    [(null? sexp) '()]
    [(and (symbol? sexp) (eqv? sexp old)) new]
    [(symbol? sexp) old]
    [else (map (lambda (e) (subst new old e)) sexp)]))

(subst 'ConanOBrian 'c '(c a a a a a a c c c ))