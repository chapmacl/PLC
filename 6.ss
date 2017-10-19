;Clay Chapman
;Assignment 6

;Problem 1
(define curry2
    (lambda (pred)
        (lambda (x)
            (lambda (y)
                (pred x y)))))
;Problem 2
(define curried-compose
    (lambda (pred1)
        (lambda (pred2)
            (lambda (x)
                (pred1 (pred2 x))))))

;Problem 3


(define compose
  (letrec ([helper 
             (lambda (lst x)
                (cond
                  [(null? (cdr lst)) ((car lst) x)]
                  [else ((car lst) (helper (cdr lst) x))]))])
  (lambda list-of-functions
    (lambda (x)
     ((car list-of-functions) (helper (cdr list-of-functions) x))))))
      
                

;Problem 4
(define make-list-c
    (letrec ([make-list
              (lambda (num ls)
                  (cond
                      [(zero? num) '()]
                      [else (cons ls (make-list (- num 1) ls))]))])
    (lambda (num)
        (lambda (ls)
            (make-list num ls)
            ))))

;Problem 5
(define let->application
  (lambda (ls)
    (cons (append '(lambda) (list (map car (cadr ls))) (cddr ls)) (map cadr (cadr ls)))))

;Problem 6
(define let*->let
  (lambda (ls)
    (append (cons 'let (list (list (caadr ls)))) 
      (list (cons 'let (cons (list (cadadr ls)) (cddr ls)))))))

;Problem 7
(define filter-in 
  (letrec ([remove
             (lambda (pred lst)
               (cond
                 [(null? lst) '()]
                 [(pred (car lst)) (cons (car lst) (remove pred (cdr lst)))]
                 [else (remove pred (cdr lst))]))
            ])
    (lambda (pred lst)
      (remove pred lst)
  )))

;Problem 8
(define filter-out 
  (letrec ([remove
             (lambda (pred lst)
               (cond
                 [(null? lst) '()]
                 [(not (pred (car lst))) (cons (car lst) (remove pred (cdr lst)))]
                 [else (remove pred (cdr lst))]))
            ])
    (lambda (pred lst)
      (remove pred lst)
  )))


;Problem 9
(define sort-list-of-symbols
  (lambda (los)
    (map string->symbol (list-sort string<? (map symbol->string los)))))

;Problem 10 
(define invert
  (letrec ([work
             (lambda (ls)
               (cond
                 [(null? ls) '()]
                 [else (cons (reverse (car ls)) (work (cdr ls)))]
                 
                 ))])
    (lambda (ls)
      (work ls))))

;Problem 11
(define vector-index
  (lambda (pred vect)
    (list-index pred (vector->list vect))))

  
    
;Helper of list-index
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
  
;Helper to replace an old item with a new one
(define replace
  (lambda (old new lst)
    (cond
      [(null? lst) '()]
      [(equal? (car lst) old) (cons new (replace old new (cdr lst)))]
      [else (cons (car lst) (replace old new (cdr lst)))]
      )))

;Helper to remove first instance of a thing
(define remove-first
  (lambda (element ls)
    (cond
      [(null? ls) '()]
      [(equal? element (car ls)) (remove-first '() (cdr ls))]
      [else (cons (car ls) (remove-first element (cdr ls)))])))