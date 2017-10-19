;Clayton Chapman
;Assignment 10

;Problem 1
(define free-vars
  (letrec ([get-free
             (lambda (exp bound)
               (cond
                 [(null? exp) '()]
                 [(equal? (car exp) 'lambda) (get-free (cddr exp) (append (cadr exp) bound))]
                 [(symbol? (car exp)) (if (not (check-dups (car exp) bound))
                  (cons (car exp) (get-free (cdr exp) bound))
                  (get-free (cdr exp) bound))]
                 [else (append (get-free (car exp) bound) (get-free (cdr exp) bound))]
                 ))])
    (lambda (exp)
      (if (symbol? exp)
          (list exp)
          (remove-dups (get-free exp '()))))))

(define bound-vars
  (letrec ([get-free
             (lambda (exp bound)
               (cond
                 [(null? exp) '()]
                 [(equal? (car exp) 'lambda) (get-free (cddr exp) (append (cadr exp) bound))]
                 [(symbol? (car exp)) (if (check-dups (car exp) bound)
                  (cons (car exp) (get-free (cdr exp) bound))
                  (get-free (cdr exp) bound))]
                 [else (append (get-free (car exp) bound) (get-free (cdr exp) bound))]
                 ))])
    (lambda (exp)
      (if (symbol? exp)
          '()
          (remove-dups (get-free exp '()))))))

;Problem 2
(define occurs-free?
  (lambda (x exp)
    (check-dups x (occurs-free-helper exp))))

(define occurs-bound?
  (lambda (x exp)
    (check-dups x (occurs-bound-helper exp))))

(define occurs-free-helper
  (letrec ([get-free
             (lambda (exp bound)
               (cond
                 [(null? exp) '()]
                 [(equal? (car exp) 'set!) (get-free (cdr exp) bound)]
                 [(equal? (car exp) 'let*) (get-free (let->application (let*->let exp)) bound)]
                 [(equal? (car exp) 'let) (get-free (let->application exp) (append (map car (cadr exp)) bound))]
                 [(equal? (car exp) 'lambda) (get-free (cddr exp) (append (cadr exp) bound))]
                 [(symbol? (car exp)) (if (not (check-dups (car exp) bound))
                  (cons (car exp) (get-free (cdr exp) bound))
                  (get-free (cdr exp) bound))]
                 [else (append (get-free (car exp) bound) (get-free (cdr exp) bound))]
                 ))])
    (lambda (exp)
      (if (symbol? exp)
          (list exp)
          (remove-dups (get-free exp '()))))))

(define occurs-bound-helper
  (letrec ([get-free
             (lambda (exp bound)
               (cond
                 [(null? exp) '()]
                 [(equal? (car exp) 'set!) (get-free (cdr exp) bound)]
                 [(equal? (car exp) 'let*) (get-free (let->application (let*->let exp)) bound)]
                 [(equal? (car exp) 'let) (get-free (let->application exp) (append (map car (cadr exp)) (append (map car (cadr exp)) bound)))]
                 [(equal? (car exp) 'lambda) (get-free (cddr exp) (append (cadr exp) bound))]
                 [(symbol? (car exp)) (if (check-dups (car exp) bound)
                  (cons (car exp) (get-free (cdr exp) bound))
                  (get-free (cdr exp) bound))]
                 [else (append (get-free (car exp) bound) (get-free (cdr exp) bound))]
                 ))])
    (lambda (exp)
      (if (symbol? exp)
          '()
          (remove-dups (get-free exp '()))))))

;Problem 3
(define lexical-address 
    (lambda (exp)
      (if (symbol? exp)
          (list ': 'free exp)
          (loop exp '()))))

(define loop
  (lambda (exp scope)
               (cond
                 [(null? exp) '()]
                 [(symbol? exp) (if (contains? scope exp) 
                                    (get-address scope exp 0)
                                    (list ': 'free exp))]
                 [(list? (car exp)) (cons (loop (car exp) scope) (loop (cdr exp) scope))]
                 [(equal? (car exp) 'if) (list 'if (loop (cadr exp) scope) (loop (caddr exp) scope) (loop (cadddr exp) scope))]
                 [(equal? (car exp) 'lambda) (append (list 'lambda (cadr exp)) (loop (cddr exp) (cons (cadr exp) scope)))]
                 [(equal? (car exp) 'let) (append (list 'let) (list (map (lambda (lst) (list (car lst) (loop (cadr lst) scope))) (cadr exp))) (loop (cddr exp) (cons (map car (cadr exp)) scope)))]
                 [(equal? (car exp) 'set!) (append (list 'set! (cadr exp)) (loop (cddr exp) scope))]
                 [else (cons (is-free? scope (car exp)) (loop (cdr exp) scope))]
                 )))

;Helper to determine if variables are free or not
(define is-free?
  (letrec ([loop
             (lambda (scope sym)
               (cond
                 [(contains? scope sym) (get-address scope sym 0)]
                 [else  (list ': 'free sym)])
               )])
    (lambda (scope lst)
      (cond
        [(null? lst) '()]
        [(symbol? lst) (loop scope lst)]
        [else (cons (loop scope (car lst)) (is-free? scope (cdr lst)))]))))

;Helper to get address
(define get-address
  (letrec ([loop
             (lambda (lst sym index)
               (cond
                 [(null? lst) (list ': 'free sym)]
                 [(equal? (car lst) sym) index]
                 [else (loop (cdr lst) sym (+ index 1))]
                 ))])
  (lambda (scope sym pos)
    (cond
      [(contains? (car scope) sym) (list ': pos (loop (car scope) sym 0))]
      [else (get-address (cdr scope) sym (+ pos 1))])
    )))
   

;Problem 4
(define un-lexical-address
  (letrec ([loop
             (lambda (exp scope)
               (cond
                 [(null? exp) '()]
                 [(list? (car exp)) (cons (loop (car exp) scope) (loop (cdr exp) scope))]
                 [(equal? (car exp) 'if) (list 'if (loop (cadr exp) scope) (loop (caddr exp) scope) (loop (cadddr exp) scope))]
                 [(equal? (car exp) 'lambda) (append (list 'lambda (cadr exp)) (loop (cddr exp) (cons (cadr exp) scope)))]
                 [(equal? (car exp) 'let) (append (list 'let) (list (map (lambda (lst) (list (car lst) (loop (cadr lst) scope))) (cadr exp))) (loop (cddr exp) (cons (map car (cadr exp)) scope)))]
                 [(equal? (car exp) 'set!) (append (list 'set! (cadr exp)) (loop (cddr exp) scope))]
                 [(and (equal? (car exp) ':) (equal? (cadr exp) 'free)) (caddr exp)]
                 [(equal? (car exp) ':) (list-ref (list-ref scope (cadr exp)) (caddr exp))]
                 [else (cons (car exp) (loop (cdr exp) scope))]
                 )
               )])
    (lambda (exp)
      (loop exp '()))))
   
;Helper to create a list of scope variables
(define lex-helper
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [(symbol? lst) '()]
      [(equal? (car lst) 'let) (cons (map car (cadr lst)) (lex-helper (cddr lst)))]
      [(equal? (car lst) 'lambda) (append (list (cadr lst)) (lex-helper (cddr lst)))]
      [(equal? (car lst) 'if) (append (lex-helper (cadr lst)) (lex-helper (caddr lst)) (lex-helper (cadddr lst)))]
      [(symbol? (car lst)) '()]
      [else (append (lex-helper (car lst)) (lex-helper (cdr lst)))]
      )))


(define  (flatten slist)
  (let flatten ([slist slist])
    (cond 
      [(null? slist) '()]
      [(symbol? (car slist)) 
       (cons (car slist) (flatten (cdr slist)))]
      [else (cons (flatten (car slist)) (flatten (cdr slist)))])))

;helper to check if an slist contains an item
(define (contains? slist sym)
  (let in-list? ([slist slist])
    (cond 
      [(null? slist) #f]
      [(symbol? (car slist)) 
       (or (eq? sym (car slist)) (in-list? (cdr slist)))]
      ;else car is an slist 
      [else (or (in-list? (car slist)) (in-list? (cdr slist)))])))
      
;Remove-dups
(define remove-dups
  (lambda (x)
    (cond
      [(null? x) '()]
      [(check-dups (car x) (cdr x)) (remove-dups (cdr x))]
      [else (cons (car x) (remove-dups (cdr x)))])))

;Check-dups
        ;#t=duplicates exist, #f=no duplicates
(define check-dups
  (lambda (x y)
   (if (equal? y '())
        #f
       (if (equal? x (car y))
           #t
           (check-dups x (cdr y))))))

;helper
(define let->application
  (lambda (ls)
    (cons (append '(lambda) (list (map car (cadr ls))) (cddr ls)) (map cadr (cadr ls)))))

;helper
(define let*->let
	(lambda (ls)
		(let ([arguments (cadr ls)]
			  [body (caddr ls)])
		(let*->let-helper arguments body))))
	
;helper
(define let*->let-helper
	(lambda (arguments body)
		(if(null? (cdr arguments)) 
			(list 'let arguments body)
			(list 'let (list (car arguments)) 
				(let*->let-helper (cdr arguments) body)))))