;Binomial Coefficient
(define choose
  (lambda (n r)
    (/ [fact n] [* (fact r) (fact (- n r))])))

;bt-recur
(define bt-recur
  (lambda (base pred root-pred leaf-pred)
    (letrec ([helper
               (lambda (tree)
                 (cond
                   [(null? tree) base]
                   [(not (list? tree)) (leaf-pred tree)]
                   [(and (null? (cadr tree)) (null? (caddr tree))) (leaf-pred (car tree))]
                   [else (pred (helper (cadr tree)) (root-pred (car tree)) (helper (caddr tree)))]
                   ))])
      helper)))

;Check-dups
        ;#t=duplicates exist, #f=no duplicates
(define check-dups
  (lambda (x y)
   (if (equal? y '())
        #f
       (if (equal? x (car y))
           #t
           (check-dups x (cdr y))))))

;Compose (multiple functions outside of a list)
(define compose
 (case-lambda
   [() (lambda (x) x)]
   [(first . rest)
 (let ([composed-rest (apply compose rest)])
   (lambda (x) (first (composed-rest x))))]))

;Compose (for a list of functions)
(define compose
  (letrec ([helper 
             (lambda (lst x)
                (cond
                  [(null? (cdr lst)) ((car lst) x)]
                  [else ((car lst) (helper (cdr lst) x))]))])
  (lambda list-of-functions
    (lambda (x)
     ((car list-of-functions) (helper (cdr list-of-functions) x))))))

;Contains? on slist
(define (contains? slist sym)
  (let in-list? ([slist slist])
    (cond 
      [(null? slist) #f]
      [(symbol? (car slist)) 
       (or (eq? sym (car slist)) (in-list? (cdr slist)))]
      ;else car is an slist 
      [else (or (in-list? (car slist)) (in-list? (cdr slist)))])))


;Counts Occurrences in slist
(define  (count-occurrences slist sym)
  (let count ([slist slist])
    (cond 
      [(null? slist) 0]
      [(symbol? (car slist)) 
       (+ (if (eq? sym (car slist)) 
             1 
             0)
              (count (cdr slist)))]
      ;else car is an slist 
      [else (+ (count (car slist)) (count (cdr slist)))])))

;Depth first search (graphs)
(define connected?
  (lambda (g)
    (if (equal? (length (map car g)) (length (traverse g (list (caar g)) '()))) #t #f)))
(define traverse
  (lambda (graph stack visited)
    (cond
      [(null? stack) visited]
      [(check-dups (car stack) visited) (traverse graph (cdr stack) visited)]
      [else (traverse graph  (append (cadr (list-ref graph (get-index (car stack) graph))) stack) (cons (car stack) visited))])))
(define get-index
  (lambda (x graph)
    (if (equal? x (caar graph)) 0 (+ 1 (get-index x (cdr graph))))))

;Factorial
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))

;Finds depth of each element in slist
(define (notate-depth slist) 
  (let notate ([slist slist]
              [depth 0])
    (cond 
      [(null? slist) '()]
      [(symbol? (car slist))
       (cons (list (car slist) depth) 
              (notate (cdr slist) depth))]
      ;else car is an slist 
      [else 
        (cons (notate (car slist) (+ depth 1)) 
         (notate (cdr slist) depth))])))


;Flattens a list  
(define  (flatten slist)
  (let flatten ([slist slist])
    (cond 
      [(null? slist) '()]
      [(symbol? (car slist)) 
       (cons (car slist) (flatten (cdr slist)))]
      ;else car is an slist 
      [else (append (flatten (car slist)) (flatten (cdr slist)))])))

;Largest in list
(define largest-in-list
  (lambda (lst)
    (if (andmap null? lst)
        #f
        (apply max (apply append lst)))))

;Make-BST (object style)
(define make-BST
  (lambda ()
    (let ([tree '()])
      (lambda (method . args) ;because multiple calls may be made at once
        (case method
          [(empty?) (null? tree)]
          [(insert) (set! tree (BST-insert (car args) tree))]
          [(preorder) (BST-preorder tree)]
          [(inorder) (BST-inorder tree)]
          [(height) (BST-height tree)]
          [(contains?) (BST-contains tree (car args))]
          [(display) (list (BST-preorder tree) (BST-inorder tree) tree)]
          [else "illegal method call"])))))

;Quicksort
(define qsort
    (lambda (pred lst)
      (if (null? lst)
          '()
          (sort-list pred lst)
      )))

;Set?
(define set?
  (lambda (x)
    (if (null? x)
        #t
        (if (not (check-dups (car x) (cdr x)))
        (set? (cdr x))
            #f))))

;snlist-recur
(define snlist-recur 
  (lambda (base lst-pred pred)
  (letrec ([helper
             (lambda (lst)
               (cond
                 [(null? lst) base]
                 [(not (list? (car lst)))  (lst-pred (pred (car lst)) (helper (cdr lst)))]
                 [else (lst-pred (helper (car lst)) (helper (cdr lst)))]
                 ))])
    
      helper)))

;Sort on a list of symbols
(define sort-list-of-symbols
  (lambda (los)
    (map string->symbol (list-sort string<? (map symbol->string los)))))

;Subset?
(define subset?
  (lambda (s1 s2)
   (if (null? s1)
        #t
        (if (not (check-dups (car s1) s2))
            #f
            (subset? (cdr s1) s2)))))

;Substition on slist
(define  (subst s1 s2 slist)
  (let subst ([slist slist])
    (cond 
      [(null? slist) '()]
      [(symbol? (car slist)) 
       (cons (if (eq? (car slist) s1) s2
                 (car slist))
         (subst (cdr slist)))]
      ;else car is an slist 
      [else (cons (subst (car slist)) (subst (cdr slist)))])))  

;Remove-dups
(define remove-dups
  (lambda (x)
    (cond
      [(null? x) '()]
      [(check-dups (car x) (cdr x)) (remove-dups (cdr x))]
      [else (cons (car x) (remove-dups (cdr x)))])))

;Replace item in a list
(define replace
  (lambda (old new lst)
    (cond
      [(null? lst) '()]
      [(equal? (car lst) old) (cons new (replace old new (cdr lst)))]
      [else (cons (car lst) (replace old new (cdr lst)))]
      )))

(define-syntax simple-loop
	(syntax-rules ()
		[(_ i from v to w body ...)
			(begin (let ([i v]) 
				(let loop ()
					(if (or (< i w) (= i w))
						(begin body ...
							(set! i (+ 1 i)) (loop))))))]))

(define-syntax for
	(syntax-rules (:)
		[(_ ((init ...) : test : update ...) body ...)
			(begin init ...
				(let loop ()
					(if test
						(begin body ...
								update ...
								(loop)))))]))
