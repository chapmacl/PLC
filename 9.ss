;Clay Chapman
;Assignment 9

;Problem 1

;sn-list recur method
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




(define sn-list-sum 
  (snlist-recur 0 + (lambda (y) y)))

(define sn-list-map 
  (lambda (pred lst)
    ((snlist-recur '() cons pred) lst)))

(define sn-list-paren-count
  (snlist-recur 2 + (lambda (y) 0)))

(define sn-list-reverse
  (snlist-recur '() (lambda (x y) (append y (list x))) (lambda (y) y)))

(define sn-list-occur
  (lambda (s lst)
  ((snlist-recur 0 + (lambda (y) (if (equal? s y) 1 0))) lst)))

(define sn-list-depth
  (snlist-recur 1 (lambda (x y) (max (+ 1 x) y)) (lambda (x) 0)))

;Problem 2
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

(define bt-sum 
  (bt-recur 0 + (lambda (x) 0) (lambda (x) x)))

(define bt-inorder
  (bt-recur '() append list (lambda (x) '())))

;Problem 3
(define make-c...r 
  (lambda (str)
    (lambda (x)
      ((apply compose (map eval (map string->symbol (map (lambda (x) (list->string (list #\c x #\r))) (string->list str)))))x))))


;Helper compose function
(define compose
 (case-lambda
   [() (lambda (x) x)]
   [(first . rest)
 (let ([composed-rest (apply compose rest)])
   (lambda (x) (first (composed-rest x))))]))


;Problem 4
(define make-slist-leaf-iterator
  (lambda (slist)
  (let ([stack (make-stack)])
    (stack 'push slist)
     (letrec ([loop
                (lambda ()
       (if (stack 'empty?)
           #f
       (let ([top (stack 'pop)])
       (cond
         [(null? top) (loop)]
         [(symbol? top) top]
         [(list? top) (begin (stack 'push (cdr top)) (stack 'push (car top)) (loop))]
         ))))])
       loop))))


(define make-stack
(lambda ()
 (let ([stk '()])
 (lambda (msg . args )
 (case msg ; Scheme's case is a similar to switch in some other languages.
 [(empty?) (null? stk)]
 [(push) (set! stk (cons (car args) stk))]
 [(pop) (let ([top (car stk)])
 (set! stk (cdr stk))
top)]
 [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))


