;Clay Chapman
;Jenner Wile
;Assignment 14


(load "chez-init.ss")

;---------------+
;				|
;	DATATYPES	|
;				|
;---------------+

; parsed expression
(define-datatype expression expression?
  	[var-exp (id symbol?)]
  	[lit-exp (id literal?)]
  	[lambda-exp (id symbol?) (bodies (list-of expression?))]
  	[lambda-exp-var (ids (list-of symbol?)) (bodies (list-of expression?))]
  	[lambda-exp-improper (ids (list-of symbol?)) (bodies (list-of expression?))]
  	[if-then-exp (body expression?) (then expression?)]
  	[if-then-else-exp (body expression?) (then expression?) (els expression?)]
  	[cond-exp (expressions (list-of expression?)) (bodies (list-of expression?))]
  	[set!-exp (id symbol?) (r-val-exp expression?)]
  	[let-exp (ids (list-of symbol?)) (var-bodies (list-of expression?)) (bodies (list-of expression?))]
  	[let-name-exp (name symbol?) (ids (list-of symbol?)) (var-bodies (list-of expression?)) (bodies (list-of expression?))]
  	[let*-exp (ids (list-of symbol?)) (var-bodies (list-of expression?)) (bodies (list-of expression?))]
  	[letrec-exp (ids (list-of symbol?)) (var-bodies (list-of expression?)) (bodies (list-of expression?))]
        [and-exp (bodies (list-of expression?))]
        [or-exp (bodies (list-of expression?))]
        [begin-exp (bodies (list-of expression?))]
        [while-exp (condition expression?) (bodies (list-of expression?))]
    [case-exp (item expression?) (keys (lambda (x) (map (lambda (y) (or (list-of (list-of expression?)) (eqv? y 'else))) x))) (bodies (list-of expression?))]
  	[app-exp (rator expression?) (rands (list-of expression?))])

;; environment type definitions

(define scheme-value?
	(lambda (x) #t))

(define-datatype environment environment?
	[empty-env-record]
	[extended-env-record
		(syms (list-of symbol?))
		(vals (list-of scheme-value?))
		(env environment?)])

; datatype for procedures. At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
	[prim-proc
		(name symbol?)]
        [closure (params (list-of symbol?)) (bodies (list-of expression?)) (env environment?)]
        [closure-var (param symbol?) (bodies (list-of expression?)) (env environment?)]
        [closure-i (param (list-of symbol?)) (bodies (list-of expression?)) (env environment?)])
  


;---------------+
;				|
;	 PARSER		|
;				|
;---------------+

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
			                [(not (all-symbols? (cadr datum))) (eopl:error 'parse-exp "Incorrect argument for lambda ~s" datum)]
			                [(and (list? (cadr datum)) (andmap literal? (cadr datum))) (lambda-exp-var (cadr datum) (map parse-exp (cddr datum)))]
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
	               
	                
	                [(eqv? (car datum) 'let)
		                (cond
			                [(< (length datum) 3) (eopl:error 'parse-exp "Not enough arguments for let statement ~s" datum)]
			                [(not (list? (cadr datum))) (eopl:error 'parse-exp "Incorrect variable binding structure in let statement ~s" datum)]
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
			                [else (letrec-exp (map car (cadr datum)) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))])]
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
		    [lit-exp (id) id]
		    [lambda-exp (id bodies) (cons* 'lambda id (map unparse-exp bodies))]
		    [lambda-exp-var (ids bodies) (cons* 'lambda ids (map unparse-exp bodies))]
		    [lambda-exp-improper (ids bodies) (cons* 'lambda (p-to-i ids) (map unparse-exp bodies))]
		    [if-then-exp (body then) (list 'if (unparse-exp body) (unparse-exp then))]
		    [if-then-else-exp (body then els) (list 'if (unparse-exp body) (unparse-exp then) (unparse-exp els))]
		    [set!-exp (id r-val-exp) (cons* 'set! id (unparse-exp r-val-exp))]
		    [let-exp (ids var-bodies bodies) (cons* 'let (map (lambda (x y) (list x y)) ids (map unparse-exp var-bodies)) (map unparse-exp bodies))]
		    [let-name-exp (name ids var-bodies bodies)  (cons* 'let name (map (lambda (x y) (list x y)) ids (map unparse-exp var-bodies)) (map unparse-exp bodies))]
		    [let*-exp (ids var-bodies bodies) (cons* 'let* (map (lambda (x y) (list x y)) ids (map unparse-exp var-bodies)) (map unparse-exp bodies))]
		    [letrec-exp (ids var-bodies bodies) (cons* 'letrec (map (lambda (x y) (list x y)) ids (map unparse-exp var-bodies)) (map unparse-exp bodies))]
		    [app-exp (rator rands) (cons* (unparse-exp rator) (map unparse-exp rands))]
		    [begin-exp (bodies) (cons* 'begin (map unparse-exp bodies))]
            [or-exp (bodies) (cons* 'or (map unparse-exp bodies))]
            [and-exp (bodies) (cons* 'and (map unparse-exp bodies))]
            [while-exp (condtion bodies) (cons* 'while (unparse-exp condition) (map unparse-exp bodies))]
            [case-exp (item keys bodies) (cons* 'case (unparse-exp item) (map (lambda (x y) (list x y)) keys (map unparse-exp bodies)))]
     		[cond-exp (expressions bodies) (cons* 'cond (map (lambda (x y) (list x y)) (map unparse-exp expressions) (map unparse-exp bodies)))])))

;---------------+
;				|
;  ENVIRONMENTS |
;				|
;---------------+

(define empty-env
	(lambda ()
		(empty-env-record)))

(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms vals env)))

(define list-find-position
	(lambda (sym los)
		(list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
	(lambda (pred ls)
		(cond
			[(null? ls) #f]
			[(pred (car ls)) 0]
			[else (let ([list-index-r (list-index pred (cdr ls))])
					(if (number? list-index-r)
						(+ 1 list-index-r)
						#f))])))

(define apply-env
	(lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
		(cases environment env
			[empty-env-record ()
				(fail)]
			[extended-env-record (syms vals env)
				(let ([pos (list-find-position sym syms)])
					(if (number? pos)
						(succeed (list-ref vals pos))
						(apply-env env sym succeed fail)))])))


;-------------------+
;					|
; SYNTAX EXPANSION	|
;					|
;-------------------+

(define syntax-expand
  (lambda (exp)
    
    (cases expression exp
		    [var-exp (id) (var-exp id)]
		    [lit-exp (id) (lit-exp id)]
		    [lambda-exp (id bodies) (lambda-exp (cadr exp) (map syntax-expand (cddr exp)))]
		    [lambda-exp-var (ids bodies) (lambda-exp-var (cadr exp) (map syntax-expand (cddr exp)))]
		    [lambda-exp-improper (ids bodies)  (lambda-exp-improper (i-to-p (cadr exp)) (map syntax-expand (cddr exp)))]
		    [if-then-exp (body then) (if-then-exp (syntax-expand (cadr exp)) (syntax-expand (caddr exp)))]
		    [if-then-else-exp (body then els) (if-then-else-exp (syntax-expand (cadr exp)) (syntax-expand (caddr exp)) (syntax-expand (cadddr exp)))]
		    [set!-exp (id r-val-exp) (set!-exp (cadr exp) (syntax-expand (caddr exp)))]
		    [let-exp (ids var-bodies bodies) (app-exp (lambda-exp-var ids (map syntax-expand bodies)) var-bodies)]
		    [let-name-exp (name ids var-bodies bodies) (let-name-exp name ids var-bodies bodies)]
		    [let*-exp (ids var-bodies bodies) 
                (if (eq? (length ids) 1) 
                    (app-exp (lambda-exp-var ids bodies) var-bodies)
                    (app-exp (lambda-exp-var (list (car ids)) (list (syntax-expand (list 'let*-exp (cdr ids) (cdr var-bodies) bodies)))) (list (car var-bodies))))]
		    [letrec-exp (ids var-bodies bodies) (letrec-exp ids var-bodies bodies)]
		    [app-exp (rator rands) (app-exp rator rands)]
            [or-exp (bodies)
                (let loop ([bodies bodies])
                    (if (eq? (length bodies) 0)
                        (parse-exp '#f)
                        (if-then-else-exp (car bodies) (car bodies) (loop (cdr bodies)))))]
            [and-exp (bodies)
                (let loop ([bodies bodies])
                    (if (eq? (length bodies) 0)
                        (parse-exp '#t)
                        (if-then-else-exp (car bodies)(loop (cdr bodies)) (car bodies))))]
            [case-exp (id keys bodies)
            	(let loop ([id id]
            			   [keys keys]
            		       [bodies bodies])
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
                    	(if-then-else-exp (car expressions) (car bodies) (loop (cdr expressions) (cdr bodies)))))])
))

(define let->application
	(lambda (expr)
		(append (list (append '(lambda) (list (map car (cadr expr))) (cddr expr))) (map cadr (cadr expr)))))

;---------------+
;				|
;  INTERPRETER	|
;				|
;---------------+

; top-level-eval evaluates a form in the global environment

(define top-level-eval
	(lambda (form env)
		(eval-exp form env)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (let ([identity-proc (lambda (x) x)])
	(lambda (exp env)
		(cases expression exp
			[lit-exp (datum) datum]
			[var-exp (id)
				(apply-env env id ; look up its value.
					(lambda (x) x) ; proc to call if id is in the environment
					(lambda () (eopl:error 'apply-env ; procedure to call if id not in env
						"variable not found in environment: ~s"
						id)))]
            [lambda-exp (id bodies) (closure-var id bodies env)]
    
			[lambda-exp-var (id bodies) (closure id bodies env)]

            [lambda-exp-improper (ids bodies) (closure-i ids bodies env)]

            [let-exp (ids var-bodies bodies) (eval-bodies bodies (extend-env ids (eval-rands var-bodies env) env))]

            [if-then-exp (body then) (if (eval-exp body env) (eval-exp then env))]

            [if-then-else-exp (body then els) (if (eval-exp body env) (eval-exp then env) (eval-exp els env))]

	        [cond-exp (expressions bodies) (doof expressions bodies env)]

	        [case-exp (item keys bodies) #f]

	        [begin-exp (bodies)
	        	(let loop ([bodies bodies])
	        		(if (eq? (length bodies) 1)
	        			(eval-exp (car bodies) env)
	        			(begin 
	        				(eval-exp (car bodies) env)
	        				(loop (cdr bodies)))))]

	        [set!-exp (id r-val-exp)
	        	(set! x 1)]

	        [while-exp (condition bodies)
	        	(if (eval-exp condition env)
	        		(begin 
	        			(eval-bodies bodies env)
	        			(eval-exp exp env)))]

			[app-exp (rator rands)
				(let ([proc-value (eval-exp rator env)]
					  [args (eval-rands rands env)])
					(apply-proc proc-value args))]
			[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)]))))




; evaluate the list of operands, putting results into a list 

(define eval-rands
	(lambda (rands env)
              		  (map (lambda (x) (eval-exp x env)) rands)))

(define eval-bodies
  (lambda (bodies env)
    (let loop ([bodies bodies])
      (if (null? (cdr bodies)) 
          (eval-exp (car bodies) env)
          (begin
            (eval-exp (car bodies) env)
            (loop (cdr bodies)))))))

; Apply a procedure to its arguments.
; At this point, we only have primitive procedures.
; User-defined procedures will be added later.

(define apply-proc
	(lambda (proc-value args)
		(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args)]
                        [closure (params bodies env) (eval-bodies bodies (extend-env params args env))]
                        [closure-var (param bodies env) (eval-bodies bodies (extend-env (list param) (list args) env))]
			[closure-i (params bodies env) (eval-bodies bodies (extend-env params (improperfy params args) env))]			
			[else (eopl:error 'apply-proc
					"Attempt to apply bad procedure: ~s"
					proc-value)])))

(define improperfy
  (lambda (par args)
    (cond
      [(null? (cdr par)) (list args)]
      [else (cons (car args) (improperfy (cdr par) (cdr args)))])))
  
(define *prim-proc-names* '(+ - * / = <= >= < > add1 sub1 zero? not cons car cdr list null? assq eq? equal? atom? length list->vector list? 
							pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr!
							vector-set! display newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr 
                            map apply member? quotient))

(define init-env				; for now, our initial global environment only contains
	(extend-env 				; procedure names. Recall that an environment associates
		*prim-proc-names*		; a value (not an expression) with an identifier.
		(map prim-proc 
			*prim-proc-names*)
		(empty-env)))

; Usually an interpreter must define each
; built-in procedure individually. We are "cheating" a little bit.

(define apply-prim-proc
	(lambda (prim-proc args)
		(case prim-proc
			[(+) (apply + args)]
			[(-) (apply - args)]
			[(*) (apply * args)]
			[(/) (apply / args)]
			[(quotient) (quotient (car args) (cadr args))]
			[(add1) (+ (car args) 1)]
			[(sub1) (- (car args) 1)]
			[(cons) (cons (car args) (cadr args))]
			[(=) (= (car args) (cadr args))]
			[(zero?) (zero? (car args))]
			[(not) (not (car args))]
			[(<=) (<= (car args) (cadr args))]
			[(>=) (>= (car args) (cadr args))]
                        [(<) (< (car args) (cadr args))]
			[(>) (> (car args) (cadr args))]
			[(car) (car (car args))]
			[(cdr) (cdr (car args))]
			[(list) (cons (car args) (cdr args))]
			[(null?) (null? (car args))]
			[(assq) (assq (car args) (cdr args))]
			[(eq?) (eq? (car args) (cadr args))]
			[(equal?) (equal? (car args) (cadr args))]
			[(atom?) (atom? (car args))]
			[(length) (length (car args))]
			[(list->vector) (list->vector (car args))]
			[(list?) (list? (car args))]
			[(pair?) (pair? (car args))]
			[(procedure?) (proc-val? (car args))]
			[(vector->list) (vector->list (car args))]
			[(vector) (apply vector args)]
		;[(make-vector) (makes a vector of symbol x n times)]
			[(vector-ref) (vector-ref (car args) (cadr args))]
			[(vector?) (vector? (car args))]
			[(number?) (number? (car args))]
			[(symbol?) (symbol? (car args))]
			[(set-car!) (set-car! (car args) (cadr args))]
			[(set-cdr!) (set-cdr! (car args) (cadr args))]
			[(vector-set!) (vector-set! (car args) (cadr args) (caddr args))]
			[(display) (display (car args))]
			[(newline) (newline)]
			[(caar) (caar (car args))]
			[(cadr) (cadr (car args))]
			[(cdar) (cdar (car args))]
			[(cddr) (cddr (car args))]
			[(caaar) (caaar (car args))]
			[(caadr) (caadr (car args))]
			[(cadar) (cadar (car args))]
			[(caddr) (caddr (car args))]
			[(cdaar) (cdaar (car args))]
			[(cdadr) (cdadr (car args))]
			[(cddar) (cddar (car args))]
			[(cdddr) (cdddr (car args))]
			[(member?) (member (car args) (cadr args))]
            [(apply) (apply-proc (car args) (cadr args))]
            [(map) (map (lambda (x) (apply-proc (car args) (list x))) (cadr args))]
			[else (error 'apply-prim-proc
					"Bad primitive procedure name: ~s"
					prim-proc)])))

(define rep 		; "read-eval-print" loop.
	(lambda ()
		(display "--> ")
		;; notice that we don't save changes to the environment...
		(let ([answer (top-level-eval (syntax-expand (parse-exp (read))) init-env)])
			;; TODO: are there answers that should display differently?
			(eopl:pretty-print answer) (newline)
			(rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
	(lambda (x) (top-level-eval (syntax-expand (parse-exp x)) init-env)))