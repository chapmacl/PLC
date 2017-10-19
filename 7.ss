;Clay Chapman
;Assignment 7

;Problem 1
(define vector-append-list
    (lambda (v lst)
    (let ([new-v (make-vector (+ (vector-length v) (length lst)) '())])
                  (do ((i 0 (+ i 1)))
                      ((= i (vector-length new-v)))
                      (cond
                          [(> (vector-length v) i) (vector-set! new-v i (vector-ref v i))]
                          [else (vector-set! new-v i (list-ref lst (- i (vector-length v))))]))
    
        new-v)))

;Problem 2
(define qsort
    (lambda (pred lst)
      (if (null? lst)
          '()
          (sort-list pred lst)
      )))

;Helper to do work on the list
(define sort-list
             (lambda (pred lst)
               (cond
                 [(null? lst) '()]
                 [else  (append  (sort-list pred (filter (lambda (y) (pred y (car lst))) lst)) 
                          (list (car lst))  
                          (sort-list pred (cdr (filter (lambda (y) (not (pred y (car lst)))) lst))))])
               ))

;Problem 3
(define connected?
  (lambda (g)
    (if (equal? (length (map car g)) (length (traverse g (list (caar g)) '()))) #t #f)))


;Helper to traverse a graph
(define traverse
  (lambda (graph stack visited)
    (cond
      [(null? stack) visited]
      [(check-dups (car stack) visited) (traverse graph (cdr stack) visited)]
      [else (traverse graph  (append (cadr (list-ref graph (get-index (car stack) graph))) stack) (cons (car stack) visited))])))

;Helper to find index of a graph
(define get-index
  (lambda (x graph)
    (if (equal? x (caar graph)) 0 (+ 1 (get-index x (cdr graph))))))
                                                        


        

;Problem 4
(define reverse-it
  (letrec ([work
             (lambda (old new)
               (cond
                 [(null? old) new]
                 [else (work (cdr old) (cons (car old) new))]))])
    (lambda (x)
      (work x '())
      )))

;Problem 5
(define empty-BST
  (lambda ()
  '()))

(define BST-element car)

(define BST-right caddr)

(define BST-left cadr)

(define empty-BST?
  (lambda (t)
    (null? t)))

(define BST-insert
  (letrec ([ins
             (lambda (e tree)
               (cond
                 [(null? tree) (list e '() '())]
                 [(= e (car tree)) tree]
                 [(< e (car tree)) (list (car tree) (ins e (cadr tree)) (caddr tree))]
                 [(> e (car tree)) (list (car tree) (cadr tree) (ins e (caddr tree)))]))
             ])
    (lambda (e tree)
      
      (cond
        [(null? tree)  (list e '() '())]
        [(= e (car tree)) tree]
        [(< e (car tree)) (list (car tree) (ins e (cadr tree)) (caddr tree))]
        [else  (list (car tree) (cadr tree) (ins e (caddr tree)))]))))
    
(define BST-inorder
  (letrec ([inorder
             (lambda (tree)
               (cond
                 [(null? tree) '()]
                 [else (append (inorder (cadr tree)) (list (car tree)) (inorder (caddr tree)))]
            ))])
  (lambda (tree)
    (if (empty-BST? tree) '() (inorder tree)))))

(define BST-contains?
  (letrec ([ins
             (lambda (e tree)
               (cond
                 [(null? tree) #f]
                 [(= e (car tree)) #t]
                 [(< e (car tree)) (ins e (cadr tree))]
                 [(> e (car tree)) (ins e (caddr tree))]))
             ])
    (lambda (tree e)
      
      (cond
        [(null? tree) #f]
        [(= e (car tree)) #t]
        [(< e (car tree)) (ins e (cadr tree))]
        [else (ins e (caddr tree))]))))

(define BST-insert-nodes
  (lambda (bst nums)
    (if (null? nums) 
        bst 
        (BST-insert-nodes (BST-insert (car nums) bst) (cdr nums)))))
    
(define BST?
  (lambda (bst)
    (cond
      [(null? bst) #t]
      [(not (list? bst)) #f]
      [(not (= 3 (length bst))) #f]
      [(not (and (list? (cadr bst)) (list? (caddr bst)) )) #f]
      [(not (andmap number? (BST-inorder bst))) #f]
      [(not (apply < (BST-inorder bst))) #f]
      [else #t]
      )
    ))

;Problem 6
(define map-by-position
  (lambda (pred-ls ls)
    (map 
      (lambda (p x) 
        (p x)) 
      pred-ls ls)))



(define bt-max
  (lambda (tree)
    (cond
      [(null? tree) '()]
      [(number? tree) tree] 
      [(= 1 (length tree)) (car tree)]    
      [else (max (bt-max (cadr tree)) (bt-max (caddr tree)))]
       )))

(define bt-max-interior
  (letrec ([helper 
             (lambda (tree)
               (cond
                 [(number? tree) (list #f tree tree)]
                 [else 
                   (let test ([left (helper (cadr tree))]
                              [right (helper (caddr tree))])
                     (cond
                       
                       [(and (>= (cadr left) (+ (cadr left) (cadr right))) (number? (cadr tree)) (number? (caddr tree))) 
                         (list (car tree) (+ (cadr left) (cadr right)) (+ (cadr left) (cadr right)))]
                       [(and (> (cadr right) (+ (cadr left) (cadr right))) (number? (cadr tree)) (number? (caddr tree)))
                        (list (car tree) (+ (cadr left) (cadr right)) (+ (cadr left) (cadr right)))]                      
                       [(>= (cadr left) (+ (cadr left) (cadr right))) left]
                       [(> (cadr right) (+ (cadr left) (cadr right))) right]
                       [else (list (car tree) (+ (cadr left) (cadr right)) (+ (cadr left) (cadr right)))])
                     )
                   ]
                 ))])
    (lambda (tree)
      (if (and (number? (cadr tree)) (number? (caddr tree)))
          (car tree)
          (let test2 ([left (helper (cadr tree))]
                     [right (helper (caddr tree))])
            (cond
             
              [(number? (caddr tree)) (car left)]
              [(number? (cadr tree)) (car right)]
              [(>= (cadr left) (+ (cadr left) (cadr right))) (car left)]
              [(> (cadr right) (+ (cadr left) (cadr right))) (car right)]
              [else (cond
                      [(> (cadr right) (cadr left)) (car right)]
                      [else (car left)]
                      )]
              ))))))


(define bt-inorder-list
  (lambda (tree)
    (cond
      [(null? tree) '()]
      [(number? tree) '()] 
      [(= 1 (length tree)) '()]    
      [else (append (bt-inorder-list (cadr tree)) (list (car tree)) (bt-inorder-list (caddr tree)))]
       )))

(define bt-leaf-sum
  (lambda (tree)
    (cond
      [(null? tree) 0]
      [(number? tree) tree] 
      [(= 1 (length tree)) (car tree)]    
      [else (+ (bt-leaf-sum (cadr tree)) (bt-leaf-sum (caddr tree)))]
       )))
   



;Helper finds duplicates in a list
;#t=duplicates exist #f=no duplicates
(define check-dups
  (lambda (x lst)
   (if (equal? lst '())
        #f
       (if (equal? x (car lst))
           #t
           (check-dups x (cdr lst))))))