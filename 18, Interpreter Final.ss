; $$$$$$\ $$\       $$$$$$\$$\     $$\        $$$$$$\ $$\   $$\ $$$$$$\ $$$$$$$\ $$\      $$\ $$$$$$\ $$\   $$\ 
;$$  __$$\$$ |     $$  __$$\$$\   $$  |      $$  __$$\$$ |  $$ $$  __$$\$$  __$$\$$$\    $$$ $$  __$$\$$$\  $$ |
;$$ /  \__$$ |     $$ /  $$ \$$\ $$  /       $$ /  \__$$ |  $$ $$ /  $$ $$ |  $$ $$$$\  $$$$ $$ /  $$ $$$$\ $$ |
;$$ |     $$ |     $$$$$$$$ |\$$$$  /        $$ |     $$$$$$$$ $$$$$$$$ $$$$$$$  $$\$$\$$ $$ $$$$$$$$ $$ $$\$$ |
;$$ |     $$ |     $$  __$$ | \$$  /         $$ |     $$  __$$ $$  __$$ $$  ____/$$ \$$$  $$ $$  __$$ $$ \$$$$ |
;$$ |  $$\$$ |     $$ |  $$ |  $$ |          $$ |  $$\$$ |  $$ $$ |  $$ $$ |     $$ |\$  /$$ $$ |  $$ $$ |\$$$ |
;\$$$$$$  $$$$$$$$\$$ |  $$ |  $$ |          \$$$$$$  $$ |  $$ $$ |  $$ $$ |     $$ | \_/ $$ $$ |  $$ $$ | \$$ |
; \______/\________\__|  \__|  \__|           \______/\__|  \__\__|  \__\__|     \__|     \__\__|  \__\__|  \__|                                                                                                              

;                 )      )         (                  (      (           
;              ( /(   ( /(         )\ )     (  (      )\ )   )\ )        
;   (    (     )\())  )\())  (    (()/(     )\))(   '(()/(  (()/(   (    
;   )\   )\   ((_)\  ((_)\   )\    /(_))   ((_)()\ )  /(_))  /(_))  )\   
;  ((_) ((_)   _((_)  _((_) ((_)  (_))     _(())\_)()(_))   (_))   ((_)  
; _ | | | __| | \| | | \| | | __| | _ \    \ \((_)/ /|_ _|  | |    | __| 
;| || | | _|  | .` | | .` | | _|  |   /     \ \/\/ /  | |   | |__  | _|  
; \__/  |___| |_|\_| |_|\_| |___| |_|_\      \_/\_/  |___|  |____| |___| 
                                                                        
(load "chez-init.ss")


; parsed expression
(define-datatype expression expression?
  	[var-exp (id symbol?)]
  	[lit-exp (id literal?)]
  	[lambda-exp (id (lambda (x) (or (symbol? x) (and (eq? (length x) 2) (eqv? (car x) 'ref) (symbol? (cadr x)))))) (bodies (list-of expression?))]
  	[lambda-exp-var (ids (list-of (lambda (x) (or (symbol? x) (and (eq? (length x) 2) (eqv? (car x) 'ref) (symbol? (cadr x))))))) (bodies (list-of expression?))]
  	[lambda-exp-improper (ids (list-of (lambda (x) (or (symbol? x) (and (eq? (length x) 2) (eqv? (car x) 'ref) (symbol? (cadr x))))))) (bodies (list-of expression?))]
  	[if-then-exp (body expression?) (then expression?)]
  	[if-then-else-exp (body expression?) (then expression?) (els expression?)]
  	[cond-exp (expressions (list-of expression?)) (bodies (list-of expression?))]
  	[set!-exp (id symbol?) (r-val-exp expression?)]
        [define-exp (id symbol?) (r-val-exp expression?)]
  	[let-exp (ids (list-of symbol?)) (var-bodies (list-of expression?)) (bodies (list-of expression?))]
  	[let-name-exp (name symbol?) (ids (list-of symbol?)) (var-bodies (list-of expression?)) (body (list-of expression?))]
  	[let*-exp (ids (list-of symbol?)) (var-bodies (list-of expression?)) (bodies (list-of expression?))]
        [and-exp (bodies (list-of expression?))]
        [or-exp (bodies (list-of expression?))]
        [begin-exp (bodies (list-of expression?))]
    [while-exp (condition expression?) (bodies (list-of expression?))]
    [case-exp (item expression?) (keys (lambda (x) (map (lambda (y) (or (list-of (list-of expression?)) (eqv? y 'else))) x))) (bodies (list-of expression?))]
  	[letrec-exp (proc-names (list-of symbol?)) (bodies (list-of expression?)) (letrec-body expression?)]
  	[app-exp (rator expression?) (rands (list-of expression?))]
        )

;; environment type definitions

(define scheme-value?
	(lambda (x) #t))

(define-datatype environment environment?
	[empty-env-record]
	[extended-env-record
		(syms (list-of symbol?))
		(vals (list-of scheme-value?))
		(env environment?)]
	[recursively-extended-env-record
		(proc-names (list-of symbol?))
		(bodies (list-of expression?))
		(env environment?)])

; datatype for procedures. At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
	[prim-proc
		(name symbol?)]
        [closure (params (list-of symbol?)) (bodies (list-of expression?)) (env environment?)]
        [closure-ref (params (list-of (lambda (x) (or (symbol? x) (and (eq? (length x) 2) (eqv? (car x) 'ref) (symbol? (cadr x))))))) (bodies (list-of expression?)) (env environment?)]
        [closure-var (param symbol?) (bodies (list-of expression?)) (env environment?)]
        [closure-i (param (list-of symbol?)) (bodies (list-of expression?)) (env environment?)]
	[continuation-proc (k continuation?)]
        [escaper-proc (p proc-val?)])
  


;Cell stuff
(define (cell value)
	(box value))
(define (cell? obj)
	(box? obj))
(define (cell-ref cell)
 	(unbox cell))
(define (cell-set! cell value)
 	(set-box! cell value))

(define deref cell-ref)

(define set-ref! cell-set!)



(define parse-exp
	(lambda (datum)
		(cond
		    [(symbol? datum) (var-exp datum)]
		    [(literal? datum) (lit-exp datum)]
		    [(and (pair? datum) (not (list? datum))) (eopl:error 'parse-exp "Improper list ~s" datum)]
		    [(pair? datum) 
		    	(cond
			    	[(eqv? (car datum) 'quote) (list 'lit-exp (cadr datum))]
		                [(eqv? (car datum) 'lambda) 
			                (cond 
				                [(< (length datum) 3) (eopl:error 'parse-exp "Incorrect number of arguments for lambda ~s" datum)]
				                ;[(not (all-symbols? (cadr datum))) (eopl:error 'parse-exp "Incorrect argument for lambda ~s" datum)]
				                [(and (list? (cadr datum)) (andmap (lambda (x) (or (symbol? x) (and (eq? (length x) 2) (eqv? (car x) 'ref) (symbol? (cadr x))))) (cadr datum))) (lambda-exp-var (cadr datum) (map parse-exp (cddr datum)))]
				                [(and (pair? (cadr datum)) (not (list? (cadr datum)))) (lambda-exp-improper (i-to-p (cadr datum)) (map parse-exp (cddr datum)))]
				                [else (lambda-exp (cadr datum) (map parse-exp (cddr datum)))])]
		                [(eqv? (car datum) 'if)
			                (cond
				                [(< (length datum) 3) (eopl:error 'parse-exp "Not enough arguments for if statement ~s" datum)]
				                [(> (length datum) 4) (eopl:error 'parse-exp "Too many arguments for if statement ~s" datum)]
				                [(null? (cdddr datum)) (if-then-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)))]
				                [else (if-then-else-exp (parse-exp (cadr datum)) (parse-exp (caddr datum)) (parse-exp (cadddr datum)))])]
		                [(eqv? (car datum) 'set!)
			                (cond
				                [(not (= (length datum) 3)) (eopl:error 'parse-exp "Incorrect number of arguments for set! ~s" datum)]
				                [else (set!-exp (cadr datum) (parse-exp (caddr datum)))])] 
	                        [(eqv? (car datum) 'define)
			                (cond
				                [(not (= (length datum) 3)) (eopl:error 'parse-exp "Incorrect number of arguments for define ~s" datum)]
				                [else (define-exp (cadr datum) (parse-exp (caddr datum)))])]
		                [(eqv? (car datum) 'let)
			                (cond
				                [(< (length datum) 3) (eopl:error 'parse-exp "Not enough arguments for let statement ~s" datum)]
				                [(not (list? (cadr datum))) ; named let
				                	(let-name-exp (cadr datum) (map car (caddr datum)) (map parse-exp (map cadr (caddr datum))) (map parse-exp (cdddr datum)))]
				                [(not (andmap list? (cadr datum))) (eopl:error 'parse-exp "Non-list inside of variable binding structure in let statement ~s" datum)]
				                [(not (andmap (lambda (x) (equal? (length x) 2)) (cadr datum))) (eopl:error 'parse-exp "Variable bind is incorrect in let statement ~s" datum)]
				                [(not (andmap symbol? (map car (cadr datum)))) (eopl:error 'parse-exp "Attempting to bind non-symbol in let statement ~s" datum)]
				                [(not (andmap expression? (map parse-exp (map cadr (cadr datum))))) (eopl:error 'parse-exp "Attemping to bind variable to non-expression in let statement ~s" datum)]
				                [else (let-exp (map car (cadr datum)) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))])]
		                [(eqv? (car datum) 'let*)
			                (cond
				                [(< (length datum) 3) (eopl:error 'parse-exp "Not enough arguments for let* statement ~s" datum)]
				                [(not (list? (cadr datum))) (eopl:error 'parse-exp "Incorrect variable binding structure in let* statement ~s" datum)]
				                [(not (andmap list? (cadr datum))) (eopl:error 'parse-exp "Non-list inside of variable binding structure in let* statement ~s" datum)]
				                [(not (andmap (lambda (x) (equal? (length x) 2)) (cadr datum))) (eopl:error 'parse-exp "Variable bind is incorrect in let* statement ~s" datum)]
				                [(not (andmap symbol? (map car (cadr datum)))) (eopl:error 'parse-exp "Attempting to bind non-symbol in let* statement ~s" datum)]
				                [(not (andmap expression? (map parse-exp (map cadr (cadr datum))))) (eopl:error 'parse-exp "Attemping to bind variable to non-expression in let* statement ~s" datum)]
				                [else (let*-exp (map car (cadr datum)) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))])]
		                [(eqv? (car datum) 'letrec)
			                (cond
				                [(< (length datum) 3) (eopl:error 'parse-exp "Not enough arguments for letrec statement ~s" datum)]
				                [(not (list? (cadr datum))) (eopl:error 'parse-exp "Incorrect variable binding structure in letrec statement ~s" datum)]
				                [(not (andmap list? (cadr datum))) (eopl:error 'parse-exp "Non-list inside of variable binding structure in letrec statement ~s" datum)]
				                [(not (andmap (lambda (x) (equal? (length x) 2)) (cadr datum))) (eopl:error 'parse-exp "Variable bind is incorrect in letrec statement ~s" datum)]
				                [(not (andmap symbol? (map car (cadr datum)))) (eopl:error 'parse-exp "Attempting to bind non-symbol in letrec statement ~s" datum)]
				                [(not (andmap expression? (map parse-exp (map cadr (cadr datum))))) (eopl:error 'parse-exp "Attemping to bind variable to non-expression in letrec statement ~s" datum)]
				                [else 
				                	(letrec-exp (map car (cadr datum)) (map parse-exp (map cadr (cadr datum))) (parse-exp (caddr datum)))])]
		                [(eqv? (car datum) 'cond)
		                	(cond-exp (map parse-exp (map car (cdr datum))) (map parse-exp (map cadr (cdr datum))))]
	                    [(eqv? (car datum) 'or)
	                        (or-exp (map parse-exp (cdr datum)))]
	                    [(eqv? (car datum) 'and)
	                        (and-exp (map parse-exp (cdr datum)))]
	                    [(eqv? (car datum) 'begin)
	                    	(begin-exp (map parse-exp (cdr datum)))]
	                    [(eqv? (car datum) 'while)
	                    	(while-exp (parse-exp (cadr datum)) (map parse-exp (cddr datum)))]
	                    [(eqv? (car datum) 'case)
	                    	(case-exp (parse-exp (cadr datum)) (map car (cddr datum)) (map parse-exp (map cadr (cddr datum))))]
                            
		                [else (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))])]
		    [else (eopl:error 'parse-exp "Invalid concrete syntax ~s" datum)])))

(define literal?
  	(lambda (x)
    	(or (number? x) (boolean? x) (vector? x) (string? x) (not (pair? x)) (eqv? x 'else))))

;Convert improper list to proper
(define i-to-p
  	(lambda (lst)
    	(cond
      		[(pair? lst) (cons (car lst) (i-to-p (cdr lst)))]
      		[else (list lst)])))

;Convert proper list to improper
 (define p-to-i
    (lambda (lst)
      	(cond
        	[(null? (cdr lst)) (car lst)]
        	[else (cons (car lst) (p-to-i (cdr lst)))])))

(define all-symbols?
  	(lambda (lst)
    	(cond
      		[(null? lst) #t]
      		[(symbol? lst) #t]
      		[(not (symbol? (car lst))) #f]
      		[else (all-symbols? (cdr lst))])))

(define unparse-exp
	(lambda (exp)
	    (cases expression exp
		    [var-exp (id) id]
		    [ref-exp (id) id]
		    [lit-exp (id) id]
		    [lambda-exp (id bodies) (cons* 'lambda id (map unparse-exp bodies))]
		    [lambda-exp-var (ids bodies) (cons* 'lambda ids (map unparse-exp bodies))]
		    [lambda-exp-improper (ids bodies) (cons* 'lambda (p-to-i ids) (map unparse-exp bodies))]
		    [if-then-exp (body then) (list 'if (unparse-exp body) (unparse-exp then))]
		    [if-then-else-exp (body then els) (list 'if (unparse-exp body) (unparse-exp then) (unparse-exp els))]
		    [set!-exp (id r-val-exp) (cons* 'set! id (list (unparse-exp r-val-exp)))]
            [define-exp (id r-val-exp) (cons* 'define id (unparse-exp r-val-exp))]
		    [let-exp (ids var-bodies bodies) (cons* 'let (map (lambda (x y) (list x y)) ids (map unparse-exp var-bodies)) (map unparse-exp bodies))]
		    [let-name-exp (name ids var-bodies body)  (cons* 'let name (map (lambda (x y) (list x y)) ids (map unparse-exp var-bodies)) (unparse-exp body))]
		    [let*-exp (ids var-bodies bodies) (cons* 'let* (map (lambda (x y) (list x y)) ids (map unparse-exp var-bodies)) (map unparse-exp bodies))]
		    [letrec-exp (proc-names bodies letrec-body) (cons* 'letrec (map (lambda (x y) (list x y)) proc-names (map unparse-exp bodies)) (list (unparse-exp letrec-body)))]
		    [app-exp (rator rands) (cons* (unparse-exp rator) (map unparse-exp rands))]
		    [begin-exp (bodies) (cons* 'begin (map unparse-exp bodies))]
            [or-exp (bodies) (cons* 'or (map unparse-exp bodies))]
            [and-exp (bodies) (cons* 'and (map unparse-exp bodies))]
            [while-exp (condtion bodies) (cons* 'while (unparse-exp condition) (map unparse-exp bodies))]
            [case-exp (item keys bodies) (cons* 'case (unparse-exp item) (map (lambda (x y) (list x y)) keys (map unparse-exp bodies)))]
     		[cond-exp (expressions bodies) (cons* 'cond (map (lambda (x y) (list x y)) (map unparse-exp expressions) (map unparse-exp bodies)))])))


                                                                               

(define empty-env
	(lambda ()
		(empty-env-record)))

(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms (map cell vals) env)))

(define extend-env-recursively
	(lambda (proc-names bodies old-env k)
		(apply-k k (recursively-extended-env-record
			proc-names bodies old-env))))

(define extend-env-ref
	(lambda (syms vals env)
   
		(extended-env-record (map (lambda (x y) 
		                           (if (and (list? x) (eqv? (car x) 'ref))
      		                               (apply-env-ref env 
				                            y
				                           (lambda (v) (cell v))
				                           (lambda () (eopl:error 'exended-env-record "bad reference")))
					       (cell x))) syms vals) vals env)))

(define list-find-position
	(lambda (sym los)
		(list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
	(lambda (pred ls)
		(cond
			[(null? ls) #f]
			[(pred (car ls)) 0]
			[else 
				(let ([list-index-r (list-index pred (cdr ls))])
					(if (number? list-index-r)
						(+ 1 list-index-r)
						#f))])))

(define apply-env-ref
	(lambda (env sym k fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
		(cases environment env
		[empty-env-record ()
			(global-check global-env sym k fail)]
		[extended-env-record (syms vals env)
			(let ([pos (list-find-position sym syms)])
				(if (number? pos)
					(apply-k k (list-ref vals pos))
					(apply-env-ref env sym k fail)))]
		[recursively-extended-env-record (procnames bodies old-env)
			(let ([pos (list-find-position sym procnames)])
				(if (number? pos)
					(cond
                        [(eqv? (car (list-ref bodies pos)) 'lambda-exp) (cell (closure-var (cadr (list-ref bodies pos)) (caddr (list-ref bodies pos)) env))]
                        [(eqv? (car (list-ref bodies pos)) 'lambda-exp-var) (cell (closure (cadr (list-ref bodies pos)) (caddr (list-ref bodies pos)) env))]
                        [(eqv? (car (list-ref bodies pos)) 'lambda-exp-improper) (cell (closure-i (cadr (list-ref bodies pos)) (caddr (list-ref bodies pos)) env))])
					(apply-env-ref old-env sym k fail)))])))

(define global-check
	(lambda (env sym k fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
		(begin 
			(cases environment env
				[empty-env-record () (fail)]
				[extended-env-record (syms vals env)
					(let ([pos (list-find-position sym syms)])
						(if (number? pos)
							(apply-k k (list-ref vals pos))
							(global-check env sym k fail)))]
				[recursively-extended-env-record (procnames bodies old-env) (apply-k k '())]))))

(define apply-env
  	(lambda (env sym k fail)
    	(cases environment env
			[empty-env-record ()
				(fail)]
			[extended-env-record (syms vals env)
				(let ([pos (list-find-position sym syms)])
					(if (number? pos)
						(apply-k k (deref (list-ref vals pos)))
						(apply-env env sym k fail)))]
			[recursively-extended-env-record (procnames bodies old-env)
				(let ([pos (list-find-position sym procnames)])
					(if (number? pos)
						(cond
                            [(eqv? (car (list-ref bodies pos)) 'lambda-exp) (apply-k k (closure-var (cadr (list-ref bodies pos)) (caddr (list-ref bodies pos)) env))]
                            [(eqv? (car (list-ref bodies pos)) 'lambda-exp-var) (apply-k k (closure (cadr (list-ref bodies pos)) (caddr (list-ref bodies pos)) env))]
                            [(eqv? (car (list-ref bodies pos)) 'lambda-exp-improper) (apply-k k (closure-i (cadr (list-ref bodies pos)) (caddr (list-ref bodies pos)) env))])
						(apply-env old-env sym k fail)))])))

(define *prim-proc-names* '(+ - * / = <= >= < > add1 sub1 zero? not cons car cdr list null? assq eq? eqv? equal? atom? length list->vector list? 
							pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
							vector-set! display newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr 
                            map apply member? quotient list-tail append call/cc exit-list escaper))

(define init-env				; for now, our initial global environment only contains
	(extend-env 				; procedure names. Recall that an environment associates
		*prim-proc-names*		; a value (not an expression) with an identifier.
		(map prim-proc 
			*prim-proc-names*)
		(empty-env)))

(define global-env 
		init-env)

(define reset-global-env
    (lambda () (set! global-env init-env)))

(define print-global-env
	(lambda () (begin (display global-env) (newline))))



(define syntax-expand
  (lambda (exp)
    (cases expression exp
	    [var-exp (id) (var-exp id)]
	    [lit-exp (id) (list 'lit-exp id)]
	    [lambda-exp (id bodies) (lambda-exp id (map syntax-expand bodies))]
	    [lambda-exp-var (ids bodies) (lambda-exp-var ids (map syntax-expand bodies))]
	    [lambda-exp-improper (ids bodies)  (lambda-exp-improper ids (map syntax-expand bodies))]
	    [if-then-exp (body then) (if-then-exp (syntax-expand (cadr exp)) (syntax-expand (caddr exp)))]
	    [if-then-else-exp (body then els) (if-then-else-exp (syntax-expand (cadr exp)) (syntax-expand (caddr exp)) (syntax-expand (cadddr exp)))]
	    [set!-exp (id r-val-exp) (set!-exp id (syntax-expand r-val-exp))]
            [define-exp (id r-val-exp) (define-exp id (syntax-expand r-val-exp))]
		[let-exp (ids var-bodies bodies) (app-exp (lambda-exp-var ids (map syntax-expand bodies)) var-bodies)]
		[let-name-exp (name ids var-bodies body) 
	    	(syntax-expand (letrec-exp (list name) 
		                   (list (lambda-exp-var ids body)) 
		                   (app-exp (parse-exp name) var-bodies)))]
	    [let*-exp (ids var-bodies bodies) (if (eq? (length ids) 1) (app-exp (lambda-exp-var ids bodies) var-bodies) (app-exp (lambda-exp-var (list (car ids)) (list (syntax-expand (list 'let*-exp (cdr ids) (cdr var-bodies) bodies)))) (list (car var-bodies))))]
	    [letrec-exp (proc-values bodies letrec-body) 
	    	(letrec-exp proc-values (map syntax-expand bodies) (syntax-expand letrec-body))]
	    [app-exp (rator rands) (app-exp rator (map syntax-expand rands))]
           
        
        [or-exp (bodies)
            (let loop ([bodies bodies])
                (if (eq? (length bodies) 0)
                    (parse-exp '#f)
                    (syntax-expand (let-exp (list 'val) (map syntax-expand (list (car bodies))) (list (if-then-else-exp (syntax-expand (var-exp 'val)) (syntax-expand (var-exp 'val)) (loop (cdr bodies))))))))]
        
        [and-exp (bodies)
            (let loop ([bodies bodies])
                (if (eq? (length bodies) 0)
                    (parse-exp '#t)
                    (if-then-else-exp (car bodies) (loop (cdr bodies)) (car bodies))))]
        
        [case-exp (id keys bodies)
           	(let loop ([id id] [keys keys] [bodies bodies])
		        (cond
        			[(eq? (length keys) 0)
        			 	(list 'var-exp 'unspecified)]
        			[(and (eq? (length keys) 1) (eqv? (car keys) 'else))
        				(car bodies)]
        		    [else 
        				(if-then-else-exp (list 'app-exp '(var-exp member?) (list id (list 'lit-exp (car keys)))) (car bodies) (loop id (cdr keys) (cdr bodies)))]))]
	    [begin-exp (bodies)
	    	(begin-exp (map syntax-expand bodies))]
	    [while-exp (condition bodies)
	    	(while-exp (syntax-expand condition) (map syntax-expand bodies))]
	    [cond-exp (expressions bodies) 
       		(let loop ([expressions expressions]
       			   	   [bodies bodies])
	            (if (eq? (length expressions) 1)
	            	(car bodies)
	            	(if-then-else-exp (car expressions) (car bodies) (loop (cdr expressions) (cdr bodies)))))])))

(define let->application
	(lambda (expr)
		(append (list (append '(lambda) (list (map car (cadr expr))) (cddr expr))) (map cadr (cadr expr)))))



(define-datatype continuation continuation?
	[init-k]
	[test-k (then-exp expression?)
			(else-exp expression?)
			(env environment?)
			(k continuation?)]
	[rator-k (rands (list-of expression?))
			 (env environment?)
			 (k continuation?)]
	[rands-k (proc-value scheme-value?)
			 (k continuation?)]
	[mapped-car-k (fn procedure?)
				  (cdr-lst (list-of expression?))
				  (k continuation?)]
	[mapped-cdr-k (car-val scheme-value?)
				  (k continuation?)]
	[evaled-car-k (bodies (list-of expression?))
				  (env environment?)
				  (k continuation?)]
	[evaled-cdr-k (bodies (list-of expression?))
				  (env environment?)
				  (k continuation?)]
	[set!-local-k (env environment?)
				  (sym literal?)
				  (k continuation?)]
	[set!-global-k (val scheme-value?)
				   (sym literal?)
				   (k continuation?)]
	[set!-k (val scheme-value?)
			(k continuation?)]
	[set-r-val-k (r-val-exp expression?)
				 (env environment?)
				 (k continuation?)]
	[define-k (ids (list-of literal?))
			  (k continuation?)]
	[deref-k (k continuation?)]
	[recursive-extend-k (body expression?)
						(k continuation?)])

(define apply-k
	(lambda (k v)
		(cases  continuation k 
			[init-k () v]
			[test-k (then-exp else-exp env k)
				(if v
					(eval-exp then-exp env k)
					(eval-exp else-exp env k))]
			[rator-k (rands env k)
				(eval-rands rands env (rands-k v k))]
			[rands-k (proc-value k)
				(apply-proc proc-value v k)]
			[mapped-car-k (fn cdr-lst k)
				(map-cps fn cdr-lst (mapped-cdr-k v k))]
			[mapped-cdr-k (car-val k)
				(apply-k k (cons car-val v))]
			[evaled-car-k (bodies env k)
				(eval-bodies bodies env
					(evaled-cdr-k bodies env k))]
			[evaled-cdr-k (bodies env k)
				(apply-k k v)]
			
			[set!-local-k (env sym k)
				(apply-env-ref env sym 
					(set!-k v k) 
					(lambda () (apply-k (set!-global-k v sym k) '())))]
			[set!-global-k (val sym k)
				(apply-env-ref global-env sym 
					(set!-k val k) 
					(eopl:error 'set!-exp "set! fail'd"))]
			
			[set!-k (val k)
				(apply-k k (set-ref! val v))]
			

			[set-r-val-k (r-val-exp env k)
				(eval-exp r-val-exp env 
					(set!-k v k))]

			[define-k (ids k)
				(apply-k k (set! global-env (extend-env ids (list v) global-env)))]
			[deref-k (k)
				(apply-k k (deref v))]
			[recursive-extend-k (body k)
				(eval-exp body v k)])))

(define map-cps
	(lambda (fn lst k)
		(if (null? lst)
			(apply-k k lst)
			(fn (car lst) 
				(mapped-car-k fn (cdr lst) k)))))




                                                                                       
(define top-level-eval
	(lambda (form env k)
		(eval-exp form env k)))

(define eval-exp
	(lambda (exp env k)
		(cases expression exp
		    [lit-exp (datum) (apply-k k datum)]
		    
		    [var-exp (id)
				(apply-env env id ; look up its value.
				  	k ; proc to call if id is in the environment
				  	(lambda () 
		                (apply-env-ref global-env id 
		                    (deref-k k)
		                    (lambda () (eopl:error 'apply-env "You goofed")))))]

        	[lambda-exp (id bodies) (apply-k k (closure-var id bodies env))]

    		[lambda-exp-var (id bodies) 
    			(apply-k k (closure (map (lambda (x) (if (and (list? x) (eq? (length x) 2) (eqv? (car x) 'ref) (symbol? (cadr x)))
                                                           (cadr x) x)) id) bodies env))]

            [lambda-exp-improper (ids bodies) (apply-k k (closure-i ids bodies env))]

            ; [let-exp (ids var-bodies bodies) (eval-bodies bodies (extend-env ids (eval-rands var-bodies env) env))]

            ;[letrec-exp (proc-names bodies letrec-body)
            ;	(eval-exp letrec-body (extend-env-recursively proc-names bodies env) k)]

            [letrec-exp (proc-names bodies letrec-body)
            	(extend-env-recursively proc-names bodies env
            		(recursive-extend-k letrec-body k))]

            [if-then-exp (body then) 
            	(eval-exp body env 
            		(test-k then (list 'lit-exp #f) env k))]

            [if-then-else-exp (body then els) 
            	(eval-exp body env 
            		(test-k then els env k))]

		    [begin-exp (bodies)
		        (eval-bodies bodies env k)]

        	;[set!-exp (id r-val-exp) 
        	;	(set-ref! 
            ;        (apply-env-ref env id 
            ;         	(lambda (v) v)
            ;         	(lambda () 
            ;           		(apply-env-ref global-env id
            ;            		(lambda (x) x)
            ;             		(lambda () (eopl:error 'set!-exp "set! fail'd")))))
            ;       (eval-exp r-val-exp env k))]

        	;[set!-exp (id r-val-exp)
        	;	(eval-exp r-val-exp env 
        	;		(set!-local-k env id k))]

        	[set!-exp (id r-val-exp)
        		(apply-env-ref env id 
        			(set-r-val-k r-val-exp env k)
        			(lambda () 
        				(apply-env-ref global-env id
        					(set-r-val-k r-val-exp global-env k)
        					(eopl:error 'set!-exp "we dun goofed"))))]

        	;[define-exp (id r-val-exp) 
        	;	(set! global-env (extend-env (list id) (list (eval-exp r-val-exp env k)) global-env))]

        	[define-exp (id r-val-exp)
        		(eval-exp r-val-exp env 
        			(define-k (list id) k))]

    		[while-exp (condition bodies)
        		(if (eval-exp condition env k)
	        		(begin 
	        			(eval-bodies bodies env k)
	        			(eval-exp exp env k)))]

        	;[app-exp (rator rands)
			;	(let ([proc-value (eval-exp rator env k)]
			;		  [args (eval-rands rands env k)])
			;	  	(apply-proc proc-value args k))]

			[app-exp (rator rands)
				(eval-exp rator env 
					(rator-k rands env k))]

    		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))


(define eval-rands
	(lambda (rands env k)
        (map-cps (lambda (x k) (eval-exp x env k)) rands k)))

;(define eval-bodies
;  	(lambda (bodies env k)
;    	(let loop ([bodies bodies])
;      		(if (null? (cdr bodies)) 
;	          	(eval-exp (car bodies) env k)
;	          	(begin
;		            (eval-exp (car bodies) env k)
;		            (loop (cdr bodies)))))))

(define eval-bodies
	(lambda (bodies env k)
		(if (null? (cdr bodies))
			(eval-exp (car bodies) env k)
			(eval-exp (car bodies) env 
				(evaled-car-k (cdr bodies) env k)))))

(define apply-proc
	(lambda (proc-value args k) 
		(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args k)]
                        [closure (params bodies env) (eval-bodies bodies (extend-env params args env) k)]
                        [closure-ref (params bodies env) (eval-bodies bodies (extend-env-ref params args env) k)]
                        [closure-var (param bodies env) (eval-bodies bodies (extend-env (list param) (list args) env) k)]
			[closure-i (params bodies env) (eval-bodies bodies (extend-env params (improperfy params args) env) k)]
			[continuation-proc (k) (apply-k k (car args))]		
   	                [escaper-proc (proc) (apply-proc proc args (init-k))]
			[else (eopl:error 'apply-proc
					"Attempt to apply bad procedure: ~s"
					proc-value)])))

(define improperfy
  	(lambda (par args)
  	  	(cond
		    [(null? (cdr par)) (list args)]
		    [else (cons (car args) (improperfy (cdr par) (cdr args)))])))
  
(define apply-prim-proc
	(lambda (prim-proc args k)
		(case prim-proc
			[(+) (apply-k k (apply + args))]
			[(-) (apply-k k (apply - args))]
			[(*) (apply-k k (apply * args))]
			[(/) (apply-k k (apply / args))]
			[(quotient) (apply-k k (quotient (car args) (cadr args)))]
			[(add1) (apply-k k (+ (car args) 1))]
			[(sub1) (apply-k k (- (car args) 1))]
			[(cons) (apply-k k (cons (car args) (cadr args)))]
			[(=) (apply-k k (= (car args) (cadr args)))]
			[(zero?) (apply-k k (zero? (car args)))]
			[(not) (apply-k k (not (car args)))]
			[(<=) (apply-k k (<= (car args) (cadr args)))]
			[(>=) (apply-k k (>= (car args) (cadr args)))]
            [(<) (apply-k k (< (car args) (cadr args)))]
			[(>) (apply-k k (> (car args) (cadr args)))]
			[(car) (apply-k k (car (car args)))]
			[(cdr) (apply-k k (cdr (car args)))]
			[(list) (apply-k k (cons (car args) (cdr args)))]
			[(null?) (apply-k k (null? (car args)))]
			[(assq) (apply-k k (assq (car args) (cadr args)))]
			[(eq?) (apply-k k (eq? (car args) (cadr args)))]
			[(eqv?) (apply-k k (eqv? (car args) (cadr args)))]
			[(equal?) (apply-k k (equal? (car args) (cadr args)))]
			[(atom?) (apply-k k (atom? (car args)))]
			[(length) (apply-k k (length (car args)))]
			[(list->vector) (apply-k k (list->vector (car args)))]
			[(list?) (apply-k k (list? (car args)))]
			[(pair?) (apply-k k (pair? (car args)))]
			[(procedure?) (apply-k k (proc-val? (car args)))]
			[(vector->list) (apply-k k (vector->list (car args)))]
			[(vector) (apply-k k (apply vector args))]
			;[(make-vector) (makes a vector of symbol x n times)]
			[(vector-ref) (apply-k k (vector-ref (car args) (cadr args)))]
			[(vector?) (apply-k k (vector? (car args)))]
			[(number?) (apply-k k (number? (car args)))]
			[(symbol?) (apply-k k (symbol? (car args)))]
			[(set-car!) (apply-k k (set-car! (car args) (cadr args)))]
			[(set-cdr!) (apply-k k (set-cdr! (car args) (cadr args)))]
			[(vector-set!) (apply-k k (vector-set! (car args) (cadr args) (caddr args)))]
			[(caar) (apply-k k (caar (car args)))]
			[(cadr) (apply-k k (cadr (car args)))]
			[(cdar) (apply-k k (cdar (car args)))]
			[(cddr) (apply-k k (cddr (car args)))]
			[(caaar) (apply-k k (caaar (car args)))]
			[(caadr) (apply-k k (caadr (car args)))]
			[(cadar) (apply-k k (cadar (car args)))]
			[(caddr) (apply-k k (caddr (car args)))]
			[(cdaar) (apply-k k (cdaar (car args)))]
			[(cdadr) (apply-k k (cdadr (car args)))]
			[(cddar) (apply-k k (cddar (car args)))]
			[(cdddr) (apply-k k (cdddr (car args)))]
			[(append) (apply-k k (append (car args) (cadr args)))]
			[(list-tail) (apply-k k (list-tail (car args) (cadr args)))]
			[(member?) (apply-k k (member (car args) (cadr args)))]
                        [(apply) (apply-proc (car args) (cadr args) k)]
                        [(map) (apply-k k (map (lambda (x) (apply-proc (car args) (list x) k)) (cadr args)))]
                        [(newline) (newline)]
                        [(display) (display (car args))]
                        [(printf) (printf (car args))]
                        [(call/cc) (apply-proc (car args) (list (continuation-proc k)) k)]
                        [(exit-list) (apply-k (init-k) args)]
                        [(escaper) (apply-k k (escaper-proc (car args)))] 
			[else (error 'apply-prim-proc
					"Bad primitive procedure name: ~s"
					prim-proc)])))

(define rep 		
	(lambda ()
		(display "--> ")
		(let ([answer (top-level-eval (syntax-expand (parse-exp (read))) init-env (init-k))])
			(eopl:pretty-print answer) (newline)
			(rep))))

(define eval-one-exp
	(lambda (x) 
		(top-level-eval (syntax-expand (parse-exp x)) init-env (init-k))))