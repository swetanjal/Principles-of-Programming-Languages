
#lang racket
(require eopl)

(define-datatype tree tree?
  [leaf (key number?)]
  [node (key number?) (left-parent tree?) (right-parent tree?)])

(define reduce
  (lambda(f init tr)
    (cases tree tr
      (leaf(v) v)
      (node(v l r)(f (f (reduce f init l) (reduce f init r) v)))
     )))

(define treeduce
  (lambda (f init tr)
    (f (reduce f init tr) init)))

(define tree/map
  (lambda(f tr)
    (cases tree tr
      (leaf(v) (leaf (f v)))
      (node(v l r) (node (f v) (tree/map f l)(tree/map f r)))
     )))

(define path
  (lambda(n tr)
    (cases tree tr
      (leaf(v)'())
      (node(v l r)
           (if (eqv? n v)
               '()
               (if (< n v)
                   (cons 'left (path n l))
                   (cons 'right (path n r)))))
     )
   ))

(define reduce2
  (lambda(f lst)
    (if (null? lst)
        '()
        (f (reduce2 f (cdr lst)) (list(car lst)) ))))

(define reverse
  (lambda(lst)
    (reduce2 append lst)))

(define increment
    (lambda (x)
      (+ x 1)))

(define map2
  (lambda(f lst)
    (if (null? lst)
        '()
        (cons (list (f (car (car lst))) (car (cdr (car lst)))) (map2 f (cdr lst))))))

(define g
  (lambda(lst1 lst2)
    (if (null? lst2)
        (list lst1)
        (append (list lst1) (map2 increment lst2)))))

(define swap
  (lambda(ls)
    (if (null? ls)
        '()
        (if (null? (cdr ls))
            (list (car ls))
            (if (> (car ls) (car (cdr ls)))
                (append (list (car (cdr ls))) (swap (append (list (car ls)) (cdr (cdr ls)))))
                (append (list (car ls)) (swap (cdr ls))))))))

(define bubble-sort
  (lambda(lst)
    (if(null? lst)
       '()
       (append (bubble-sort (cdr (reverse (swap lst)))) (list (car (reverse (swap lst)))))
     )))
