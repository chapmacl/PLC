;Clay Chapman
;Assignment 8

;Problem 1
(define  (slist-map proc slist)
  (let slist-map ([slist slist])
    (cond 
      [(null? slist) '()]
      [(symbol? (car slist)) (cons (proc (car slist)) (slist-map (cdr slist)))]
      [else (cons (slist-map (car slist))         
  (slist-map (cdr slist)))])))  
            

(define  (slist-reverse slist)
  (let slist-reverse ([slist slist])
    (cond 
          [(null? slist) '()]
          [(list? (car slist)) (append (slist-reverse (cdr slist)) (list (slist-reverse (car slist))))]
          [else (append (slist-reverse (cdr slist)) (list (car slist)))])))

(define (slist-depth slist) 
  (let notate ([slist slist] 
               [depth 0])
    (cond 
      [(null? slist) (+ 1 depth)]
      [(symbol? (car slist)) (max depth (notate (cdr slist) depth))]
      [else (max (notate (car slist) (+ 1 depth)) (notate (cdr slist) depth))])))


(define (slist-paren-count slist) 
  (let notate ([slist slist])
    (cond 
      [(null? slist) 2]
      [(symbol? (car slist)) (notate (cdr slist))]
      [else (+ (notate (car slist)) (notate (cdr slist)))])))
 
(define (slist-symbols-at-depth slist d) 
  (let notate ([slist slist] 
               [depth 1])
    (cond 
      [(null? slist) '()]
      [(and (symbol? (car slist)) (equal? depth d)) (cons (car slist) (notate (cdr slist) depth))]
      [(symbol? (car slist)) (notate (cdr slist) depth)]
      [else (append (notate (car slist) (+ 1 depth)) (notate (cdr slist) depth))])))
 

;Problem 2
(define group-by-two
    (lambda (lst)
        (cond
            [(null? lst) '()]
            [(null? (cdr lst)) (list (list (car lst)))]
            [else (cons (list (car lst) (cadr lst)) (group-by-two (cddr lst)))]
        )))

;Problem 3
(define group-by-n
  (letrec ([helper
             (lambda (lst n)
               (cond
                 [(or (null? lst) (zero? n)) '()]
                 [else (cons (car lst) (helper (cdr lst) (- n 1)))]
                 ))])
    (lambda (lst n)
      (cond
        [(null? lst) '()]
        [(< (length lst) n) (list lst)]
        [else (cons (helper lst n) (group-by-n (list-at-n lst n) n))]
        ))))
  
    
;Helper to return a list at index n
(define list-at-n
  (letrec ([loop
             (lambda (lst n)
               (cond
                 [(null? lst) '()]
                 [(not (zero? n)) (loop (cdr lst) (- n 1))]
                 [else lst]
                 ))])
  (lambda (lst n)
    (if (< (length lst) n) 
        '()
        (loop lst n)))))
    
    

;Problem 4
(define  (subst-leftmost new old slist equality-pred?)
  (car (let loop ([slist slist])
    (cond [(null? slist) (list '() #f)]
          [(and (symbol? (car slist)) (equality-pred? old (car slist)))
	   (list (cons new (cdr slist)) #t)]
    	  [(symbol? (car slist))
	  (let ([result (loop (cdr slist))])
                        (list (cons (car slist) (car result)) (cadr result)))]
          [else 
             (let ([result (loop (car slist))])
                    (cond
                      [(cadr result) (list (cons (car result) (cdr slist)) #t)]
                      [else (let ([result (loop (cdr slist))])
                        (list (cons (car slist) (car result)) (cadr result)))]))]
      ))))