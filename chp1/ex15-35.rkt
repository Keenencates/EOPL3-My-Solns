#lang racket

(require rackunit)

;;testing constants
(define conan "Conan-o'Brian")
(define co "Conan")
(define br "o'Brian")
(define cobr (list co br))
(define brco (list br co))

(define conan10 (build-list 10 (const conan)))
(define cobr10 (build-list 10 (const cobr)))
(define brco10 (build-list 10 (const brco)))
(define conan-nest (map (lambda (x) (list x)) conan10))
(define cobr-alt (flatten (build-list 5 (const cobr))))
(define brco-alt (flatten (build-list 5 (const brco))))
;ex1.15

;; Int -> SchemeVal -> Listof(SchemeVal)
(define (duple n x)
  (duple-helper '() n x))

;; Listof(SchemeVal) -> Int -> SchemeVal -> Listof(SchemeVal)
(define (duple-helper lst n x)
  (cond
    [(zero? n) lst]
    [else (duple-helper (cons x lst)(- n 1) x)]))

(check-equal? (duple 0 conan) '() "duple 0 is empty?")
(check-equal? (duple 10 conan) conan10 "duple works as intended")

;ex1.16

;;Listof(Pairof(SchemeVal SchemeVal)) -> Listof(Pairof(SchemeVal SchemeVal))
;;usage: swaps first and second of each pair
(define (invert lst)
  (cond
    [(empty? lst) lst]
    [else (cons (list (second (first lst))
                      (first (first lst)))
                (invert (rest lst)))]))

(check-equal? (invert '()) '())
(check-equal? (invert cobr10) brco10)

;ex1.17

;;Listof(SchemeVal) -> Listof(Listof(SchemeVal))
(define (down lst)
  (cond
    [(empty? lst) '()]
    [(list? lst) (cons (list (first lst)) (down (rest lst)))]))

(define test1 `(co (br (co)) itsame))
(define test1-sol `((co) ((br (co)))(itsame)))
(check-equal? (down conan10) conan-nest)
(check-equal? (down test1) test1-sol)

; ex1.18

;;Sym → Sym → Listof(Sym) → Listof(Sym)

(define (swapper s1 s2 slst)
  (cond
    [(empty? slst) '()]
    [(eqv? s1 (first slst)) (cons s2 (swapper s1 s2 (rest slst)))]
    [(eqv? s2 (first slst)) (cons s1 (swapper s1 s2 (rest slst)))]
    [else (cons (first slst) (swapper s1 s2 (rest slst)))]))

(check-equal? (swapper co br cobr-alt) brco-alt)

;ex1.19

;;Listof(SchemeVal) → Int → SchemeVal → Listof(SchemeVal);
(define (list-set lst n x)
  (cond
    [(empty? lst) (if (zero? n) (list x) (error 'list-set "Out-of-Bounds Error. N to large."))]
    [(zero? n) (cons x (rest lst))]
    [else (cons (first lst)(list-set (rest lst) (- n 1) x))]))

(check-equal? (list-set (list br br br br) 1 co) (list br co br br))
(check-exn exn:fail? (lambda () (list-set (list br br br br) 5 co)))

;ex1.20

;;Sym → Listof(Sym) → Int

(define (count-occurrences s slst)
  (cond
    [(empty? slst) 0]
    [(symbol? (first slst))(if (eqv? s (first slst))
                               (+ 1 (count-occurrences s (rest slst)))
                               (count-occurrences s (rest slst)))]
    [(list? (first slst))(+ (count-occurrences s (first slst))
                            (count-occurrences s (rest slst)))]))

(check-equal? (count-occurrences 'a '(a a a a a a a a a a)) 10)
(check-equal? (count-occurrences 'x '((f x) y (((x z) () x)))) 3)
(check-equal? (count-occurrences 'w '((f x) y (((x z) x)))) 0)

;ex1.21

;Listof(Sym) → Listof(Sym) → Listof(Pairof(Sym))


(define (product lst1 lst2)
  (cond
    [(and (empty? lst1) (empty? lst2)) '()]
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [else (reverse (product-context lst1 lst2 '()))]))

;Listof(Sym) → Listof(Sym) → Listof(Pairof(Sym)) → Listof(Pairof(Sym))
(define (product-context lst1 lst2 res)
  (if (empty? lst1)
      res
      (product-context (rest lst1)
                       lst2
                       (product-single (first lst1)
                                       lst2
                                       res))))

;Sym → Listof(Sym) → Listof(Pairof(Sym)) → Listof(Pairof(Sym))
(define (product-single s1 lst2 res)
  (cond
    [(empty? lst2) res]
    [else (product-single s1
                          (rest lst2)
                          (cons (list s1 (first lst2))
                                res))]))

(check-equal? (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y)))

;ex 1.22

;Pred → Listof(SchemeVal) → Listof(SchemeVal)
(define (filter-in pred lst)
  (reverse (filter-in-helper pred lst '())))

(define (filter-in-helper pred lst res)
  (cond
    [(empty? lst) res]
    [(pred (first lst)) (filter-in-helper pred (rest lst)(cons (first lst) res))]
    [else (filter-in-helper pred (rest lst) res)]))

(check-equal? (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
(check-equal? (filter-in symbol? '(a (b c) 17 foo)) '(a foo))

;ex 1.23

(define (list-index? pred lst)
  (list-index-helper pred lst 0))

(define (list-index-helper pred lst n)
  (cond
    [(empty? lst) #f]
    [(pred (first lst)) n]
    [else (list-index-helper pred (rest lst) (+ n 1))]))

(check-equal? (list-index? number? '(a 2 (1 3) b 7)) 1)
(check-equal? (list-index? symbol? '(a (b c) 17 foo)) 0 )
(check-equal? (list-index? symbol? '(1 2 (a b) 3)) #f)

;ex 1.24

(define (every? pred lst)
  (cond
    [(empty? lst) #t]
    [(not (pred (first lst))) #f]
    [else (every? pred (rest lst))]))

(check-equal? (every? number? '(a b c 3 e)) #f)
(check-equal? (every? number? '(1 2 3 4 5)) #t)

;ex 1.25

(define (exists? pred lst)
  (cond
    [(empty? lst) #f]
    [(pred (first lst)) #t]
    [else (exists? pred (rest lst))]))

(check-equal? (exists? number? '(a b c 3 e)) #t)
(check-equal? (exists? number? '(1 2 3 4 5)) #t)
(check-equal? (exists? number? '(a b c d e)) #f)