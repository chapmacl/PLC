;Clay Chapman
;Assignment 2

;Helper method to find square numbers
(define square
  (lambda (x)
    (* x x)))

;Problem 1.a
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

;Problem 1.b
(define choose
  (lambda (n r)
    (/ [fact n] [* (fact r) (fact (- n r))])))

;Problem 2
(define range
  (lambda (m n)
    (if (< m n)
        (cons m (range (+ m 1) n))
        '())))

;Problem 3
(define set?
  (lambda (x)
    (if (null? x)
        #t
        (if (not (check-dups (car x) (cdr x)))
        (set? (cdr x))
            #f))))
        

;Helper method that compares car with cdr elements
        ;#t=duplicates exist, #f=no duplicates
(define check-dups
  (lambda (x y)
   (if (equal? y '())
        #f
       (if (equal? x (car y))
           #t
           (check-dups x (cdr y))))))

;Problem 4
(define sum-of-squares
  (lambda (lon)
    (if (null? lon)
        0
        (+ (square (car lon)) (sum-of-squares (cdr lon))))))

;Problem 5
(define make-vec-from-points
  (lambda (p1 p2)
    (list [- (list-ref p2 0) (list-ref p1 0)] 
          [- (list-ref p2 1) (list-ref p1 1)] 
          [- (list-ref p2 2) (list-ref p1 2)])))

;Problem 6
(define dot-product
  (lambda (v1 v2)
    (+ (* (list-ref v1 0) (list-ref v2 0)) 
       (* (list-ref v1 1) (list-ref v2 1)) 
       (* (list-ref v1 2) (list-ref v2 2)))))

;Problem 7
(define vec-length
  (lambda (v)
    (sqrt (+ (square (car v)) (square (cadr v)) (square (caddr v))))))

;Problem 8 
(define distance
  (lambda (p1 p2)
    (sqrt (sum-of-squares (make-vec-from-points p1 p2)))))

;Problem 9
(define cross-product
  (lambda (v1 v2)
    (list [- (* (cadr v1) (caddr v2)) (* (caddr v1) (cadr v2))]
          [- (* (caddr v1) (car v2)) (* (car v1) (caddr v2))]
          [- (* (car v1) (cadr v2)) (* (cadr v1) (car v2))])))

;Problem 10
(define parallel?
  (lambda (v1 v2)
    (if (equal? (modulo (car v1) (car v2)) 0)
        (and [equal? (* (cadr v2) (/ (car v1) (car v2))) (cadr v1)]
             [equal? (* (caddr v2) (/ (car v1) (car v2))) (caddr v1)])
         (if (equal? (modulo (car v2) (car v1)) 0)
             (and [equal? (* (cadr v1) (/ (car v2) (car v1))) (cadr v2)]
             [equal? (* (caddr v1) (/ (car v2) (car v1))) (caddr v2)])
             #f))))

;Problem 11
(define collinear?
  (lambda (p1 p2 p3)
    (equal? (cross-product (make-vec-from-points p1 p2) (make-vec-from-points p2 p3)) '(0 0 0)
      )))