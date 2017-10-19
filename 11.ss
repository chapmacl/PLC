;Clayton Chapman
;Assignment 11

;; Binary trees using define-datatype
(load "chez-init.ss") ; chez-init.ss should be in the same folder as this code.


;Problem 1
(define-syntax my-let
  (syntax-rules ()
    [(_ ((x v) ...) e1 e2 ...)
     ((lambda (x ...) e1 e2 ...) v ...)
     ]
    [(_ name ((x v) ...) e1 e2 ...)
     ((letrec ((name (lambda (x ...) e1 e2 ...))) name) v ...)
     ]))

(define-syntax my-or
  (syntax-rules ()
    [(_) #f]
    [(_ e1) e1]
    [(_ e1 e2 e3 ...) (let ([exp e1])
                        (if exp
                          exp
                          (my-or e2 e3 ...)))]))

(define-syntax +=
  (syntax-rules ()
    [(_ x v) (begin (set! x (+ x v)) x)]))

(define-syntax return-first
  (syntax-rules ()
    [(_ e1 e2 ...) (let ([e e1]) (begin e2 ... e))]))

;Problem 2
(define bintree-to-list
  (lambda (x)
    (cases bintree x
      [leaf-node (num) (list 'leaf-node num)]
      [interior-node (key left-tree right-tree) 
        (list 'interior-node key (bintree-to-list left-tree) (bintree-to-list right-tree))]
      )))

;Problem 3
(define max-interior 
    (lambda (tree)
      (car (helper tree))))

(define helper
            (lambda (tree)
               (cases bintree tree
                 [leaf-node (num) (list #f num num)]
                 [interior-node (key left-tree right-tree)
                   (let* ([left (helper left-tree)]
                              [right (helper right-tree)]
                              [total (+ (caddr left) (caddr right))]
                              [left-max (cadr left)]
                              [right-max (cadr right)])  
                   (cond
                       [(and (not (car left)) (not (car right))) (list key total total)]
                       [(and (>= left-max total) (not (car left))) (list key left-max total)]
                       [(and (> right-max total) (not (car right))) (list key right-max total)]
                       [(and (>= left-max total) (> left-max right-max)) (list (car left) left-max total)]
                       [(> right-max total) (list (car right) right-max total)]
                       [else (list key total total)]))
                   ]
                 )))

;; from EoPL, page 50
(define-datatype bintree bintree?
 (leaf-node (num integer?))
 (interior-node (key symbol?) (left-tree bintree?) (right-tree bintree?)))

;Problem 4
(define-datatype expression expression?
  [var-exp (id symbol?)]
  [lit-exp (id literal?)]
  [lambda-exp (id symbol?) (bodies (list-of expression?))]
  [lambda-exp-var (ids (list-of symbol?)) (bodies (list-of expression?))]
  [lambda-exp-improper (ids (list-of symbol?)) (bodies (list-of expression?))]
  [if-then-exp (body expression?) (then expression?)]
  [if-then-else-exp (body expression?) (then expression?) (else expression?)]
  [set!-exp (id symbol?) (r-val-exp expression?)]
  [let-exp (ids (list-of symbol?)) (var-bodies (list-of expression?)) (bodies (list-of expression?))]
  [let-name-exp (name symbol?) (ids (list-of symbol?)) (var-bodies (list-of expression?)) (bodies (list-of expression?))]
  [let*-exp (ids (list-of symbol?)) (var-bodies (list-of expression?)) (bodies (list-of expression?))]
  [letrec-exp (ids (list-of symbol?)) (var-bodies (list-of expression?)) (bodies (list-of expression?))]
  [app-exp (rator expression?) (rands (list-of expression?))]
  )

(define parse-exp
  (lambda (datum)
    (cond
      [(symbol? datum) (var-exp datum)]
      [(literal? datum) (lit-exp datum)]
      [(and (pair? datum) (not (list? datum))) (eopl:error 'parse-exp "Improper list ~s" datum)]
      [(pair? datum) (cond
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
                       
                       ;at least length 3, cadr is a list of lists of length 2, {map car {cadr datum}} are symbols
                       [(eqv? (car datum) 'let)
                        (cond
                          [(< (length datum) 3) (eopl:error 'parse-exp "Not enough arguments for let statement ~s" datum)]
                          [(not (list? (cadr datum))) (eopl:error 'parse-exp "Incorrect variable binding structure in let statement ~s" datum)]
                          [(not (andmap list? (cadr datum))) (eopl:error 'parse-exp "Non-list inside of variable binding structure in let statement ~s" datum)]
                          [(not (andmap (lambda (x) (equal? (length x) 2)) (cadr datum))) (eopl:error 'parse-exp "Variable bind is incorrect in let statement ~s" datum)]
                          [(not (andmap symbol? (map car (cadr datum)))) (eopl:error 'parse-exp "Attempting to bind non-symbol in let statement ~s" datum)]
                          [(not (andmap expression? (map parse-exp (map cadr (cadr datum))))) (eopl:error 'parse-exp "Attemping to bind variable to non-expression in let statement ~s" datum)]
                          [else (let-exp (map car (cadr datum)) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))]
                          )]
                       [(eqv? (car datum) 'let*)
                        (cond
                          [(< (length datum) 3) (eopl:error 'parse-exp "Not enough arguments for let* statement ~s" datum)]
                          [(not (list? (cadr datum))) (eopl:error 'parse-exp "Incorrect variable binding structure in let* statement ~s" datum)]
                          [(not (andmap list? (cadr datum))) (eopl:error 'parse-exp "Non-list inside of variable binding structure in let* statement ~s" datum)]
                          [(not (andmap (lambda (x) (equal? (length x) 2)) (cadr datum))) (eopl:error 'parse-exp "Variable bind is incorrect in let* statement ~s" datum)]
                          [(not (andmap symbol? (map car (cadr datum)))) (eopl:error 'parse-exp "Attempting to bind non-symbol in let* statement ~s" datum)]
                          [(not (andmap expression? (map parse-exp (map cadr (cadr datum))))) (eopl:error 'parse-exp "Attemping to bind variable to non-expression in let* statement ~s" datum)]
                          [else (let*-exp (map car (cadr datum)) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))]
                          )]
                       [(eqv? (car datum) 'letrec)
                        (cond
                          [(< (length datum) 3) (eopl:error 'parse-exp "Not enough arguments for letrec statement ~s" datum)]
                          [(not (list? (cadr datum))) (eopl:error 'parse-exp "Incorrect variable binding structure in letrec statement ~s" datum)]
                          [(not (andmap list? (cadr datum))) (eopl:error 'parse-exp "Non-list inside of variable binding structure in letrec statement ~s" datum)]
                          [(not (andmap (lambda (x) (equal? (length x) 2)) (cadr datum))) (eopl:error 'parse-exp "Variable bind is incorrect in letrec statement ~s" datum)]
                          [(not (andmap symbol? (map car (cadr datum)))) (eopl:error 'parse-exp "Attempting to bind non-symbol in letrec statement ~s" datum)]
                          [(not (andmap expression? (map parse-exp (map cadr (cadr datum))))) (eopl:error 'parse-exp "Attemping to bind variable to non-expression in letrec statement ~s" datum)]
                          [else (letrec-exp (map car (cadr datum)) (map parse-exp (map cadr (cadr datum))) (map parse-exp (cddr datum)))]
                          )]
                       [else (app-exp (parse-exp (car datum)) (map parse-exp (cdr datum)))])]
      [else (eopl:error 'parse-exp "Invalid concrete syntax ~s" datum)])))
         

(define unparse-exp
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [lit-exp (id) id]
      [lambda-exp (id bodies) (cons* 'lambda id (map unparse-exp bodies))]
      [lambda-exp-var (ids bodies) (cons* 'lambda ids (map unparse-exp bodies))]
      [lambda-exp-improper (ids bodies) (cons* 'lambda (p-to-i ids) (map unparse-exp bodies))]
      [if-then-exp (body then) (list 'if (unparse-exp body) (unparse-exp then))]
      [if-then-else-exp (body then else) (list 'if (unparse-exp body) (unparse-exp then) (unparse-exp else))]
      [set!-exp (id r-val-exp) (cons* 'set! id (unparse-exp r-val-exp))]
      [let-exp (ids var-bodies bodies) (cons* 'let (map (lambda (x y) (list x y)) ids (map unparse-exp var-bodies)) (map unparse-exp bodies))]
      [let-name-exp (name ids var-bodies bodies)  (cons* 'let name (map (lambda (x y) (list x y)) ids (map unparse-exp var-bodies)) (map unparse-exp bodies))]
      [let*-exp (ids var-bodies bodies) (cons* 'let* (map (lambda (x y) (list x y)) ids (map unparse-exp var-bodies)) (map unparse-exp bodies))]
      [letrec-exp (ids var-bodies bodies) (cons* 'letrec (map (lambda (x y) (list x y)) ids (map unparse-exp var-bodies)) (map unparse-exp bodies))]
      [app-exp (rator rands) (cons* (unparse-exp rator) (map unparse-exp rands))])))

(define literal?
  (lambda (x)
    (or (number? x) (boolean? x) (vector? x) (string? x) (not (pair? x)))))

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