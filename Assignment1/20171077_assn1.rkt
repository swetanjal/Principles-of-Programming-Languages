#lang racket
;;;; 1.15
;;;; duple: Int x SchemeVal -> List
;;;; usage: (duple n x) returns a list containing n copies of x
(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x (duple (- n 1) x)))))
;;;; 1.16
;;; invert: List of 2-lists -> List of 2-lists
;;; usage: (invert lst) returns a list with each 2-list reversed
(define invert
  (lambda(lst)
    (if (null? lst)
        '()
        (cons (list (car (cdr (car lst))) (car (car lst)))  (invert (cdr lst))))))
;;;; 1.17
;;;; down: List -> List
;;;; usage: (down lst) wraps parentheses around each top-level element of lst
(define down
  (lambda(lst)
    (if (null? lst)
        '()
        (cons (cons (car lst) '()) (down (cdr lst))))))
;;;; 1.18
;;;; swapper: Sym x Sym x S-list -> S-list
;;;; usage: (swapper s1 s2 slist) returns a list the same as slist, but with all occurrences
;;;; of s1 replaced by s2 and all occurrences of s2 replaced by s1
(define swapper
  (lambda(s1 s2 slist)
    (if (null? slist)
        '()
        (if (symbol? (car slist))
            (if (eqv? (car slist) s1)
            (cons s2 (swapper s1 s2 (cdr slist)))
            (if (eqv? (car slist) s2)
                (cons s1 (swapper s1 s2 (cdr slist)))
                (cons (car slist) (swapper s1 s2 (cdr slist)))
             ))
            (cons (swapper s1 s2 (car slist)) (swapper s1 s2 (cdr slist)))
            ))))
;;;; 1.19
;;;; list-set : List x Int x SchemeVal -> List
;;;; usage: (list-set lst n x) returns a list like lst, except that the nth element, using
;;;; zero based indexing is x
(define list-set
  (lambda(lst n x)
    (if (zero? n)
        (cons x (cdr lst))
        (cons (car lst) (list-set (cdr lst) (- n 1) x)))))
;;;; 1.20
;;;; count-occurrences : Sym x S-list -> Int
;;;; usage: (count-occurrences s slist) returns the number of occurences of s in slist
(define count-occurrences
  (lambda(s slist)
    (if(null? slist)
       0
       (if(symbol? (car slist))
          (if(eqv? (car slist) s)
             (+ 1 (count-occurrences s (cdr slist)))
             (+ 0 (count-occurrences s (cdr slist))))
          (+ (count-occurrences s (cdr slist)) (count-occurrences s (car slist))))
     )))
;;;; 1.21
;;;; joinlists : List x List -> List
;;;; usage: (joinlists lst1 lst2) takes two lists lst1 and lst2 as arguments and joins them into one list
(define joinlists
  (lambda(lst1 lst2)
    (if(null? lst1)
       (if(null? lst2)
          '()
          (cons (car lst2) (joinlists lst1 (cdr lst2))))
       (cons (car lst1) (joinlists (cdr lst1) lst2)))))

;;;; cross: symbol x List -> List
;;;; usage: (cross x lst) returns a list with the symbol x being paired with each element in lst as a list.
(define cross
  (lambda(x lst)
    (if (null? lst)
        '()
        (cons (list x (car lst)) (cross x (cdr lst)))
     )))

;;;; product: List-of-Symbols x List-of-Symbols -> List of 2-lists
;;;; usage: (product sos1 sos2) where sos1 and sos2 are each a list
;;;; of symbols without repetitions, returns a list of 2-lists that represents the Cartesian
;;;; product of sos1 and sos2. The 2-lists may appear in any order.
(define product
  (lambda(sos1 sos2)
    (if(null? sos1)
       '()
       (if (null? (cdr sos1))
           (cross (car sos1) sos2)
           (joinlists (cross (car sos1) sos2) (product (cdr sos1) sos2)))
       )))
;;;; 1.22
;;;; filter-in : Predicate x List -> List
;;;; usage: (filter-in pred lst) returns the list of those elements in lst that satisfy the predicate pred
(define filter-in
  (lambda(pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))
;;;; 1.23
;;;; list-index : Predicate x List -> SchemeVal
;;;; usage: (list-index pred lst) returns the 0-based position of the
;;;; first element of lst that satisfies the predicate pred. If no element of lst satisfies
;;;; the predicate, then list-index returns #f.
(define list-index
  (lambda(pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            0
            (if (eqv? (list-index pred (cdr lst)) #f)
                #f
                (+ 1 (list-index pred (cdr lst))))))))
;;;; 1.24
;;;; every? : Predicate x List -> boolean
;;;; usage: (every? pred lst) returns #f if any element of lst fails to satisfy pred,
;;;; and returns #t otherwise
(define every?
  (lambda(pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f))))
;;;; 1.25
;;;; exists? : Predicate x List -> boolean
;;;; usage: (exists? pred lst) returns #t if any element of lst satisfies pred,
;;;; and returns #f otherwise
(define exists?
  (lambda(pred lst)
    (if (null? lst)
        #f
        (if (eqv? (pred (car lst)) #f)
            (exists? pred (cdr lst))
            #t))))
;;;; 1.26
;;;; up: List-of Symbols -> List-of-Symbols
;;;; usage: (up slist) removes a pair of parentheses from each top-level
;;;; element of lst. If a top-level element is not a list, it is included in the result, as is.
(define up
  (lambda(slist)
    (if (null? slist)
        '()
        (if(null? (car slist))
           (up (cdr slist))
           (if (list? (car slist))
               (joinlists (car slist) (up (cdr slist)))
               (joinlists (list (car slist)) (up (cdr slist)))
            )
         ))))
;;;; 1.27
;;;; flatten : Symbol-list -> List of Symbols
;;;; usage: (flatten slist) returns a list of the symbols contained in
;;;; slist in the order in which they occur when slist is printed. Intuitively, flatten
;;;; removes all the inner parentheses from its argument.
(define flatten
  (lambda(slist)
    (if (null? slist)
        '()
        (if (list? (car slist))
            (joinlists (flatten (car slist)) (flatten (cdr slist)))
            (joinlists (list (car slist)) (flatten (cdr slist)))))))
;;;; 1.28
;;;; merge: List-of-Integers x List-of-Integers -> List-of-Ints
;;;; usage: (merge loi1 loi2) where loi1 and loi2 are lists of integers
;;;; that are sorted in ascending order, returns a sorted list of all the integers in loi1 and
;;;; loi2.
(define merge
  (lambda(loi1 loi2)
    (if (null? loi1)
        (if (null? loi2)
            '()
            (cons (car loi2) (merge loi1 (cdr loi2))))
        (if (null? loi2)
            (cons (car loi1) (merge (cdr loi1) loi2))
            (if(<= (car loi1) (car loi2))
               (cons (car loi1) (merge (cdr loi1) loi2))
               (cons (car loi2) (merge loi1 (cdr loi2))))))))
;;;; 1.29
;;;; firsthalf: Int x List-of-Ints -> List-of-Ints
;;;; usage: (firsthalf x lst) returns the first x elements from lst
(define firsthalf
  (lambda (x lst)
    (if (<= x 0)
        '()
        (cons (car lst) (firsthalf (- x 1) (cdr lst))))))
;;;; secondhalf: Int x List-of-Ints -> List-of-Ints
;;;; usage: (secondhalf x lst) returns the remaining elements from lst leaving the first x elements
(define secondhalf
  (lambda (x lst)
    (if (null? lst)
        '()
        (if (> x 0)
        (secondhalf (- x 1) (cdr lst))
        (cons (car lst) (secondhalf (- x 1) (cdr lst)))))))
;;;; sort : List-of-Ints -> List-of-Ints
;;;; usage: (sort lst) returns a list of the elements of loi in ascending order.
(define sort
  (lambda(lst)
    (if (null? lst)
        '()
        (if (eqv? (length lst) 1)
            (list(car lst))
            (merge (sort (firsthalf (floor (/ (length lst) 2)) lst)) (sort (secondhalf (floor (/ (length lst) 2)) lst)))))))

;;;;;1.30
;;;; merge/predicate : predicate x List-of-Ints x List-of-Ints -> List-of-Ints
;;;; merge/predicate takes two lists sorted according to predicate and returns a merged list sorted according to predicate
(define merge/predicate
  (lambda(pred loi1 loi2)
    (if (null? loi1)
        (if (null? loi2)
            '()
            (cons (car loi2) (merge/predicate pred loi1 (cdr loi2))))
        (if (null? loi2)
            (cons (car loi1) (merge/predicate pred (cdr loi1) loi2))
            (if(pred (car loi1) (car loi2))
               (cons (car loi1) (merge/predicate pred (cdr loi1) loi2))
               (cons (car loi2) (merge/predicate pred loi1 (cdr loi2))))))))
;;;; sort/predicate: predicate x List-of-Ints -> List-of-Ints
;;;; usage: (sort/predicate lst) returns a list of elements sorted by the predicate.
(define sort/predicate
  (lambda(pred lst)
    (if (null? lst)
        '()
        (if (eqv? (length lst) 1)
            (list(car lst))
            (merge/predicate pred (sort/predicate pred (firsthalf (floor (/ (length lst) 2)) lst)) (sort/predicate pred (secondhalf (floor (/ (length lst) 2)) lst)))))))
