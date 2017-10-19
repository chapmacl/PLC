;Clay Chapman
;Assignment 4

;Problem 1
(define multi-set?
  (lambda (obj)
    (cond 
      [(null? obj) #t]
      [(not (relation? obj)) #f]
      [(check-dups (car obj) (cdr obj)) #f]
      [(not (and (symbol? (caar obj)) (and (number? (cadar obj)) (> (cadar obj) 0)))) #f]
      [else (multi-set? (cdr obj))])))

;Problem 2
(define ms-size
  (lambda (ms)
    (if (null? ms)
        0
        (apply + (get-ms-numbers ms)))))

;Problem 3
(define matrix-ref
  (lambda (m row col)
    (list-ref (list-ref m row) col)))

;Problem 4
(define matrix?
  (lambda (obj)
    (cond
      [(null? obj) #f]
      [(not (list? obj))  #f]
      [(not (andmap list? obj)) #f]
      [(andmap null? obj) #f]
      [(not (apply = (map length obj))) #f]
      [else #t]
      )))

;Problem 5
(define matrix-transpose
  (lambda (m)
    (cond
      [(null? (car m)) '()]
      [else (cons (map car m) (matrix-transpose (map cdr m)))]
      )))

;Problem 6
(define last
  (lambda (ls)
    (if (null? ls)
        '()
        (build-list ls))))

;Helper for getting last in list
(define build-list
  (lambda (lst)
    (cond 
      [(null? (cdr lst)) (car lst)]
      [else (build-list (cdr lst))]
      )))

;Problem 7
(define all-but-last
  (lambda (ls)
    (cond
      [(null? (cdr ls)) '()]
      [else (cons (car ls) (all-but-last (cdr ls)))])))
    
      
;Helper to get a list of ms numbers
(define get-ms-numbers
  (lambda (obj)
    (cond
      [(null? obj) '()]
      [else (cons (cadar obj) (get-ms-numbers (cdr obj)))])))
      

;Helper to check if a set is a relation (modified)
(define relation?
  (lambda (obj)
    (cond
      [(not (list? obj)) #f]
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
        
;Helper method that compares car with cdr elements
        ;#t=duplicates exist, #f=no duplicates
(define check-dups
  (lambda (x y)
   (if (equal? y '())
        #f
       (if (equal? (car x) (caar y))
           #t
           (check-dups x (cdr y))))))        