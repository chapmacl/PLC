; Clay Chapman
; Assignment 1

;Problem 1
(define Fahrenheit->Celsius
  (lambda (x)
    (* (- x 32) 5/9)))

;Problem 2
(define interval-contains?
  (lambda (interval number)
    (if (<= (list-ref interval 0) (list-ref interval 1))
        (if (and [<= (list-ref interval 0) number] [>= (list-ref interval 1) number])
            #t
            #f)
        #f)))

;Problem 3
(define interval-intersects?
  (lambda (i1 i2)
    (if (and (<= (list-ref i2 0) (list-ref i1 1)) (<= (list-ref i1 0) (list-ref i2 1)))
             #t
        #f)))

;Problem 4
(define interval-union
  (lambda (i1 i2)
    (if (interval-intersects? i1 i2)
        (list(list (min (list-ref i1 0) (list-ref i2 0)) (max (list-ref i1 1) (list-ref i2 1))))
        (list i1 i2)
        )))

;Problem 5
(define divisible-by-7? 
  (lambda (num)
    (if (equal? (modulo num 7) 0)
        #t
        #f)))

;Problem 6
(define ends-with-7?
  (lambda (num)
    (if (equal? (modulo num 10) 7)
        #t
        #f)))

;Problem 7
(define 1st
  (lambda (lst)
    (car lst)))

(define 2nd
  (lambda (lst)
    (car (cdr lst))))

(define 3rd
  (lambda (lst)
    (car (cdr (cdr lst)))))