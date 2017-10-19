;Clay Chapman
;Assignment 3

;Problem 1
(define nearest-point
  (lambda (p list-of-points)
    (cond 
      [(= (distance p (car list-of-points)) 
         (min (distance p (car list-of-points)) 
           (distance p (cadr list-of-points))
           (distance p (caddr list-of-points)))) 
       (car list-of-points)]
      [(= (distance p (cadr list-of-points)) 
         (min (distance p (car list-of-points)) 
           (distance p (cadr list-of-points))
           (distance p (caddr list-of-points)))) 
       (cadr list-of-points)]
      [else (caddr list-of-points)])))

;Problem 2
(define union
  (lambda (s1 s2)
    (cond 
      [(and (null? s1) (null? s2)) '()]
      [(null? s2) (cons (car s1) (union (cdr s1) s2))]
      [(null? s1) (cons (car s2) (union s1 (cdr s2)))]
      [(check-dups (car s1) s2) (union (cdr s1) s2)]
      [(check-dups (car s2) s1) (union s1 (cdr s2))]
      [else (cons (car s1) (cons (car s2) (union (cdr s1) (cdr s2))))]
      )))

;Problem 3
(define intersection
  (lambda (s1 s2)
   (cond 
     [(or (null? s1) (null? s2)) '()]
     [(check-dups (car s1) s2) (cons (car s1) (intersection (cdr s1) (cdr s2)))]
     [(check-dups (car s2) s1) (cons (car s2) (intersection (cdr s1) (cdr s2)))]
     [else (intersection (cdr s1) (cdr s2))]
    )))
       
;Problem 4
(define subset?
  (lambda (s1 s2)
   (if (null? s1)
        #t
        (if (not (check-dups (car s1) s2))
            #f
            (subset? (cdr s1) s2)))))

;Problem 5
(define relation?
  (lambda (obj)
    (cond
      [(not (list? obj)) #f]
      [(not (equal? (modulo (length obj) 2) 0)) #f]
      [(not (check-pairs obj)) #f]
      [else #t]
      )))

;Helper that checks if elements in a list are pairs
(define check-pairs
  (lambda (x)
    (if (null? x)
        #t
        (if (pair? (car x))
              (if (equal? (length (car x)) 2)
                  (check-pairs (cdr x))
                  #f)
              #f))))
      
;Problem 6
(define domain
  (lambda (r)
        (remove-dups (build-list-of-firsts r))))

        
;Problem 7
(define reflexive?
  (lambda (r)
    (equal? (intersection (remove-dups (create-reflexive-pairs r)) r) (remove-dups (create-reflexive-pairs r)))
    ))   

;Problem 8
(define hailstone-step-count 
  (lambda (n)
    (cond
      [(equal? n 1) 0]
      [(equal? (modulo n 2) 0) (+ 1 (hailstone-step-count (/ n 2)))]
      [else (+ 1 (hailstone-step-count (+ (* n 3) 1)))])))


;Helper that builds a list using the first element of inner lists
(define build-list-of-firsts
  (lambda (r)
    (cond
      [(null? r) '()]
      [else (cons (caar r) (build-list-of-firsts (cdr r)))])))
      


;Helper to create reflexive pairs
(define create-reflexive-pairs
  (lambda (r)
    (cond
      [(null? r) '()]
      [else (cons (list (caar r) (caar r)) (create-reflexive-pairs (cdr r)))])))
     
;Helper method to find pairs
(define pairs
  (lambda (r)
    (cond 
      [(null? r) '()]
      [(equal? (caar r) (cadar r)) (cons (car r) (pairs (cdr r)))]
      [else (pairs (cdr r))])))
         
         
         
;Helper to remove duplicates from a set
(define remove-dups
  (lambda (x)
    (cond
      [(null? x) '()]
      [(check-dups (car x) (cdr x)) (remove-dups (cdr x))]
      [else (cons (car x) (remove-dups (cdr x)))])))

;Helper method to find square numbers
(define square
  (lambda (x)
    (* x x)))

;Helper to check if a list is a set
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

;Helper to calculate distance between 2 points
(define distance
  (lambda (p1 p2)
    (sqrt (sum-of-squares (make-vec-from-points p1 p2)))))

;Helper for summing squares
(define sum-of-squares
  (lambda (lon)
    (if (null? lon)
        0
        (+ (square (car lon)) (sum-of-squares (cdr lon))))))

;Helper that creates a vector
(define make-vec-from-points
  (lambda (p1 p2)
    (list [- (list-ref p2 0) (list-ref p1 0)] 
          [- (list-ref p2 1) (list-ref p1 1)] 
          [- (list-ref p2 2) (list-ref p1 2)])))
