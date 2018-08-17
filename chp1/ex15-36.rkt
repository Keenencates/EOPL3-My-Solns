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

;ex 1.26

(define (up lst)
  (cond
    [(empty? lst) '()]
    [(list? (first lst)) (append (append '() (first lst)) (up (rest lst)))]
    [else (cons (first lst) (up (rest lst)))]))

(check-equal? (up '((1 2) (3 4))) '(1 2 3 4))
(check-equal? (up '((x (y)) z)) '(x (y) z))

;ex 1.27

;Listof(SchemeVal) -> Listof(SchemeVal)
(define (mflatten slst)
  (cond
    [(empty? slst) '()]
    [(symbol? (first slst)) (cons (first slst) (mflatten (rest slst)))]
    [(list? (first slst)) (append (mflatten (first slst)) (mflatten (rest slst)))]
    [else (append (first slst) (mflatten (rest slst)))]))

(check-equal? (mflatten '(a b c)) '(a b c))
(check-equal? (mflatten '((a) () (b ()) () (c))) '(a b c))
(check-equal? (mflatten '((a b) c (((d)) e))) '(a b c d e))
(check-equal? (mflatten '(a b (() (c)))) '(a b c))

;ex 1.28

;Listof(Int) -> Listof(Int) -> Listof(Int)
(define (merge loi1 loi2)
  (merge-helper loi1 loi2 '()))

(define (merge-helper loi1 loi2 acc)
  (cond
    [(and (empty? loi1) (empty? loi2)) (reverse acc)]
    [(empty? loi1) (merge-helper loi1 (rest loi2)(cons (first loi2) acc))]
    [(empty? loi2) (merge-helper (rest loi1) loi2(cons (first loi1) acc))]
    [else (if (>= (first loi1) (first loi2))
              (merge-helper loi1 (rest loi2) (cons (first loi2) acc))
              (merge-helper (rest loi1) loi2 (cons (first loi1) acc)))]))

(check-equal? (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(check-equal? (merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91))

 ;ex 1.29

(define (msort loi)
  (cond
    [(empty? loi) '()]
    [else
     (let ([smaller (msort (filter (lambda (x)(<= x (first loi))) (rest loi)))]
           [bigger (msort (filter (lambda (x)(> x (first loi))) (rest loi)))])
       (append (append smaller (list (first loi))) bigger))]))

(check-equal? (msort '(8 2 5 2 3)) '(2 2 3 5 8))

;ex 1.30
(define (equality-wrapper pred)
  (lambda (x y)
    (or (pred x y) (eqv? x y))))

(define (msort/predicate pred loi)
    (cond
    [(empty? loi) '()]
    [else
     (let ([left (msort/predicate pred (filter (lambda (x)(pred x (first loi))) (rest loi)))]
           [right (msort/predicate pred (filter (lambda (x)(not(pred x (first loi)))) (rest loi)))])
       (append (append left (list (first loi))) right))]))

(check-equal? (msort/predicate < '(8 2 5 2 3)) '(2 2 3 5 8))
(check-equal? (msort/predicate > '(8 2 5 2 3)) '(8 5 3 2 2))

;ex 1.31

(define (leaf x)
  (list 'leaf x))

(define (interior-node symbol left right)
  (list 'interior-node symbol left right))

(define (leaf? x)
  (and (not (empty? x))
       (eqv? (first x) 'leaf)))

(define (interior-node? x)
  (and (not (empty? x))
       (eqv? (first x) 'interior-node)))

(define (lson node)
  (third node))

(define (rson node)
  (fourth node))

(define (contents-of node)
  (cond
    [(leaf? node) (second node)]
    [(interior-node? node) (second node)]))

;ex 1.32

(define (double-tree bintree)
  (cond
    [(empty? bintree) '()]
    [(leaf? bintree) (leaf (* 2 (contents-of bintree)))]
    [(interior-node? bintree) (interior-node (contents-of bintree)
                                             (double-tree (lson bintree))
                                             (double-tree (rson bintree)))]))

(define t1 (interior-node 'baz
                          (interior-node 'bar
                                         (leaf 1)
                                         (interior-node 'foo
                                                        (leaf 1)
                                                        (leaf 2)))
                          (interior-node 'biz
                                         (leaf 4)
                                         (leaf 5))))

(define t2 (interior-node 'baz
                          (interior-node 'bar
                                         (leaf 2)
                                         (interior-node 'foo
                                                        (leaf 2)
                                                        (leaf 4)))
                          (interior-node 'biz
                                         (leaf 8)
                                         (leaf 10))))

(check-equal? (double-tree (leaf 1)) (leaf 2))
(check-equal? (double-tree (leaf 2)) (leaf 4))
(check-equal? (double-tree t1) t2)

;ex 1.33

(define (mark-leaves-with-red-depth bintree)
  (mark-helper bintree 0))

(define (mark-helper bintree n)
  (cond
    [(empty? bintree) '()]
    [(leaf? bintree) (leaf n)]
    [(interior-node? bintree) (interior-node (contents-of bintree)
                                             (mark-helper (lson bintree) (if (eqv? (contents-of bintree)
                                                                                   'red) (+ n 1) n))
                                             (mark-helper (rson bintree) (if (eqv? (contents-of bintree)
                                                                                   'red) (+ n 1) n)))]))

(define tr2
  (interior-node
   'red
   (interior-node
    'bar
    (leaf 1)
    (leaf 1))
   (interior-node
    'red
    (leaf 2)
    (interior-node
     'quux
     (leaf 2)
     (leaf 2)))))

(define tr1
  (interior-node
   'red
   (interior-node
    'bar
    (leaf 26)
    (leaf 12))
   (interior-node
    'red
    (leaf 11)
    (interior-node
     'quux
     (leaf 117)
     (leaf 14)))))

(check-equal? (mark-leaves-with-red-depth tr1) tr2)

;ex 1.34

(define (path target bst)
  (path-helper target bst '()))

(define (path-helper target bst path)
  (cond
    [(empty? bst) '()]
    [(eqv? target (first bst)) path]
    [(> target (first bst)) (path-helper target (third bst) (append path '(right)))]
    [(< target (first bst)) (path-helper target (second bst) (append path '(left)))]))

(check-equal? (path 17 '( 14 ( 7 () (12 () ()))
                             (26 (20 (17 () ())
                                     ())
                                 (31 () ()))))
              '(right left left))

; ex 1.35

(define (number-leaves bintree)
  (number-leaves-helper bintree 0))

(define (count-leaves bintree)
  (cond
    [(empty? bintree) 0]
    [(leaf? bintree) 1]
    [(interior-node? bintree) (+ (count-leaves (lson bintree)) (count-leaves (rson bintree)))]))

(define (number-leaves-helper bintree n)
  (cond
    [(empty? bintree)'()]
    [(leaf? bintree) (leaf n)]
    [(interior-node? bintree)(interior-node (contents-of bintree)
                                            (number-leaves-helper (lson bintree) n)
                                            (number-leaves-helper (rson bintree) (+ n (count-leaves (lson bintree)))))]))

(check-equal? (number-leaves
               (interior-node
                'foo
                (interior-node
                 'bar
                 (leaf 26)
                 (leaf 12))
                (interior-node
                 'baz
                 (leaf 11)
                 (interior-node
                  'quux
                  (leaf 117)
                  (leaf 14)))))
              (interior-node
                'foo
                (interior-node
                 'bar
                 (leaf 0)
                 (leaf 1))
                (interior-node
                 'baz
                 (leaf 2)
                 (interior-node
                  'quux
                  (leaf 3)
                  (leaf 4)))))

;ex 1.36

(define (g f r)
  (cons f (map (lambda (x) (list (+ 1 (first x)) (second x))) r)))

(define (number-elements lst)
  (if (empty? lst)
      '()
      (g (list 0 (car lst)) (number-elements (cdr lst)))))

(number-elements '(a b c d e))