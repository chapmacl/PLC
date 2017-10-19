;Clay Chapman
;Assignment 5

;Problem 1
(define minimize-interval-list
  (lambda (ls)
    (minimize (car (list-sort helper ls)) (cdr (list-sort helper ls)))
     ))
  
;Helps minimize a list of intervals
(define minimize
  (lambda (first rest)
    (cond
      [(null? rest) (list first)]
      [(interval-intersects? first (car rest)) 
       (minimize (interval-union first (car rest)) (cdr rest))]
      [else (cons first (minimize (car rest) (cdr rest)))])))

;Problem 2
(define exists?
  (lambda (pred ls)
    (cond
      [(null? ls) #f]
      [(pred (car ls)) #t]
      [else (exists? pred (cdr ls))])))

;Problem 3
(define list-index 
  (letrec ([repeat
             (lambda(pred ls)
               (cond
                 [(null? ls) 0]
                 [(pred (car ls)) 0]
                 [else (+ 1 (repeat pred (cdr ls)))]))])
  (lambda (pred ls)
    (if (= (repeat pred ls) (length ls))
        #f
        (repeat pred ls)))))

;Problem 4
(define pascal-triangle
  (letrec ([repeat
             (lambda (n x)
               (cond
                 [(equal? n x) (list (choose n x))]
                 [else (cons (choose n x) (repeat n (+ x 1)))]
                 ))])
  (lambda (n)
    (cond
      [(zero? n) (list '(1))]
      [(not (positive? n)) '()]
      [else (cons (repeat n 0) (pascal-triangle (- n 1)))])
    )))

;Problem 5
(define product 
  (letrec ([repeat
             (lambda (x y)
               (cond
                 [(null? y) '()]
                 [else (cons (list (car x) (car y)) (repeat x (cdr y)))]))])
    (lambda (set1 set2)
      (cond
        [(or (null? set1) (null? set2)) '()]
        [else (append (repeat set1 set2) (product (cdr set1) set2))])
    )))

;Problem 6
(define max-edges
  (lambda (n)
    (/ (* n (- n 1)) 2)))

;Problem 7
(define complete? 
  (letrec ([check
             (lambda (lst n)
               (cond
                 [(null? lst) #t]
                 [(not (equal? (car lst) n)) #f]
                 [else (check (cdr lst) n)]))
            ])
  (lambda (ls)
     (check (map length (map cadr ls)) (- (length (map car ls)) 1))
    )))

;Problem 8
(define complete
  (letrec ([build
             (lambda (original point)
               (cond
                 [(null? point) '()]
                 [else (cons (list (car point) (remove-first (car point) original)) (build original (cdr point)))]
                 ))])
    (lambda (ls)
      (build ls ls))))

;Problem 9
(define replace
  (lambda (old new lst)
    (cond
      [(null? lst) '()]
      [(equal? (car lst) old) (cons new (replace old new (cdr lst)))]
      [else (cons (car lst) (replace old new (cdr lst)))]
      )))

;Problem 10
(define remove-first
  (lambda (element ls)
    (cond
      [(null? ls) '()]
      [(equal? element (car ls)) (remove-first '() (cdr ls))]
      [else (cons (car ls) (remove-first element (cdr ls)))])))

;Problem 11
(define remove-last
  (letrec ([reverse
             (lambda (x)
               (cond
                 [(null? x) '()]
                 [else (append (reverse (cdr x)) (list (car x)))]))])
    (lambda (element ls) 
      (reverse (remove-first element (reverse ls))))))
  
;Helper factorial
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

;Helper choose
(define choose
  (lambda (n r)
    (/ [fact n] [* (fact r) (fact (- n r))])))
  



;Helper finds duplicates in a list
;#t=duplicates exist #f=no duplicates
(define check-dups
  (lambda (x y)
   (if (equal? y '())
        #f
       (if (equal? x (car y))
           #t
           (check-dups x (cdr y))))))

;Helper used for sorting lists
(define helper
  (lambda (x y)
    (cond
      [(< (car x) (car y)) #t]
      [(> (car x) (car y)) #f]
      [(< (cadr x) (cadr y)) #t]
      [else #f])))

(define interval-union
  (lambda (i1 i2)
    (if (interval-intersects? i1 i2)
        (list (min (list-ref i1 0) (list-ref i2 0)) (max (list-ref i1 1) (list-ref i2 1)))
        (list i1 i2)
        )))


;Helper to find if lists intersect
(define interval-intersects?
  (lambda (i1 i2)
    (if (and (<= (list-ref i2 0) (list-ref i1 1)) (<= (list-ref i1 0) (list-ref i2 1)))
             #t
        #f)))
 
;Helper to find if a list is a subset of another list     
(define subset?
  (lambda (s1 s2)
   (if (null? s1)
        #t
        (if (not (check-dups (car s1) s2))
            #f
            (subset? (cdr s1) s2)))))

;Helper to remove duplicates from a set
(define remove-dups
  (lambda (x)
    (cond
      [(null? x) '()]
      [(check-dups (car x) (cdr x)) (remove-dups (cdr x))]
      [else (cons (car x) (remove-dups (cdr x)))])))