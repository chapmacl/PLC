;Clayton Chapman
;Final Exam

(load "chez-init.ss")

(define m-ref
  (lambda (m row col)
    (list-ref (list-ref m row) col)))



;Problem 1
(define (determinant m)
  (if (eq? (length m) 1)
      (caar m)
      (helper1 m)))

(define (helper1 m)
  (if (eq? (length m) 2)
      (2x2 m))
      (recurse m (length m))
  )

;method for recursive calculations
;(define (recurse m size)
;  (let ([work (- size 2)])
;    (letrec ([loop
;               (lambda (m i j x)
;                 (if (<= x work)
;                     recurse x times for each element in top row to reach
;                     the 2x2 stage. (+ (* (expt -1 (+ i j)) (car m) loop (cdr m)) 
                 

;gets Det of a 2x2 matrix
(define (2x2 m)
  (- (* (m-ref m 0 0) (m-ref m 1 1))
     (* (m-ref m 0 1) (m-ref m 1 0)))
  )

;builds minor matrices
(define (builder m i j)
  (build-r (build-c m 0 i) 0 j))
(define  (build-c m row i)
  (cond
    [(null? m) '()]
    [(eq? row i) (build-c (cdr m) (+ 1 row) i)]
    [else (cons (car m) (build-c (cdr m) (+ 1 row) i))]))
(define (build-r m col j)
  (letrec ([loop
             (lambda (m col j)
               (cond
                 [(null? m) '()]
                 [(eq? col j) (build-c (cdr m) (+ 1 col) j)]
                 [else (cons (car m) (build-c (cdr m) (+ 1 col) j))]))])
           (cond
             [(null? m) '()]
             [else (cons (loop (car m) col j) (build-r (cdr m) col j))])))
  

;Problem 2
(define tree-mult
  (lambda (ls)
    (tree-mult-cps ls (init-k))))
  
(define-datatype kontinuation kontinuation?
  [init-k]
  [tree-mult-cdr-k (the-car (lambda (x) (or (number? x) (list? x)))) (k kontinuation?)] 
  
  ) 
  
  
(define apply-k 
  (lambda (k v)
    (cases kontinuation k
      [init-k ()  v]
      [tree-mult-cdr-k (the-car k) ;v=tree-mult (cdr ls)
        (if (not (list? the-car))
                   (apply-k k (* the-car v))
                   (* (tree-mult-cps the-car k) v))]
	)))
  
(define tree-mult-cps
  (lambda (ls k)
    (if (null? ls)
        (apply-k k 1)
        (tree-mult-cps (cdr ls) (tree-mult-cdr-k (car ls) k)))))
          