#lang racket
;;; Problem 1
(define f
    (lambda(x)
      (lambda(y)
        (* x y))))
(define scalar-list-prod
  (lambda(x lst)
    (map (f x) lst)))
(scalar-list-prod 5 '())
(scalar-list-prod 5 '(3 4))
(scalar-list-prod 0 '(8 4 9))
;;; Problem 2
(define dot
  (lambda(x y)
    (* x y)))

(define .*
  (lambda(lst1 lst2)
    (apply + (map dot lst1 lst2))))

(.* '() '())
(.* '(2 6) '(3 4))
(.* '(3 0) '(8 4))

;;; Problem 3
(define pairx
  (lambda(x)
    (lambda(a)
      (list x a))))
(define g
  (lambda(lst)
    (lambda(x)
      (map (pairx x) lst))))
(define **
  (lambda(lst1 lst2)
    (apply append (map (g lst2) lst1))))

(** '() '(3 4))
(** '(2 7 3) '())
(** '(2) '(3 4))
(** '(2 4) '(1 3))
(** '(a b) '(a b b))