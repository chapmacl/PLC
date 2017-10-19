;Clay Chapman
;Assignment 15

;------------------------------------------------------------------------------------------------------
;Short answer problem
;------------------------------------------------------------------------------------------------------
;The function in class stores the values for every key associated with the current key (stores the values for each key between 0 and the current key)
;whereas the function written for this assignment ONLY saves the value for the current key it is working with, and no other values
;Mine is slower because it can't get stored values until it gets to a key that I have stored, but the one in class could get the stored 
;values between 0 and the previous key just by getting called on that key. 


;Helpers
(define apply-continuation 
  (lambda (k v)
    (k v)))
(define +-cps
 (lambda (a b k)
 (apply-continuation k (+ a b))))

(define cons-cps
  (lambda (x y k)
    (apply-continuation k (cons x y))))

(define snoc-cps
  (lambda (x y k)
    (if (null? y)
        (apply-continuation k (cons x y))
        (snoc-cps x (cdr y) (lambda (v) (apply-continuation k (cons (car y) v)))))))
        
(define eq-cps
  (lambda (x y k)
    (apply-continuation k (eq? x y))))

(define max-cps
  (lambda (x y k)
    (apply-continuation k (max x y))))

;------------------------------------------------------------------------------------------------------
;Problem 1
;------------------------------------------------------------------------------------------------------
(define member?-cps
  (lambda (obj lst k)
    (cond 
      [(null? lst) (apply-continuation k #f)]
      [(equal? obj (car lst)) (apply-continuation k #t)]
      [else (member?-cps obj (cdr lst) k)])))

(define set?-cps
  (lambda (ls k)
    (cond
      [(null? ls) (apply-continuation k #t)]
      [(not (pair? ls)) (apply-continuation k #f)]
      [else (set?-cps (cdr ls) 
                      (lambda (v) (member?-cps (car ls) (cdr ls) (lambda (x) (apply-continuation k (if x 
                                                                                                       #f 
                                                                                                       v))))))])))

(define 1st-cps
  (lambda (x k)
    (apply-continuation k (car x))))

(define set-of-cps
  (lambda (s k)
    (cond
      [(null? s) (apply-continuation k s)]
      [else (member?-cps (car s) (cdr s) (lambda (v) (if v
                                                         (set-of-cps (cdr s) k)
                                                         (set-of-cps (cdr s) (lambda (copy) (apply-continuation k (cons (car s) copy)))))))])))

(define map-cps
  (lambda (proc s k)
    (if (null? s)
        (apply-continuation k s)
        (proc (car s) (lambda (v) (map-cps proc (cdr s) (lambda (copy) (apply-continuation k (cons v copy)))))))))

(define domain-cps
  (lambda (x k)
    (map-cps (lambda (y c) (1st-cps y c)) x (lambda (z) (set-of-cps z k)))))
     
  
(define make-cps
  (lambda (proc)
    (lambda (x k)
      (k (proc x)))))

(define andmap-cps
  (lambda (proc x k)
    (cond 
      [(null? x) (proc x k)]
      [(null? (cdr x)) (proc (car x) k)]
      [else (proc (car x) (lambda (v) (if v
                                          (andmap-cps proc (cdr x) k)
                                          (apply-continuation k v))))])))

(define cps-snlist-recur
 (lambda (base-value item-proc-cps list-proc-cps)
   (letrec
     ([helper (lambda (ls k)
                (cond 
                  [(null? ls) (apply-continuation k base-value)]
                  [else (helper (cdr ls)
                                      
                          (lambda (x)  ;x=helper (cdr ls)
                            (1st-cps ls 
                                         
                             (lambda (y)   ;y=(car ls)
                               
                               (if (or (pair? y) (null? y))
                                       (helper y
                                      
                                         (lambda (w)    ;w=helper (car ls)
                                           
                                           (list-proc-cps w x k)))
                                       (item-proc-cps y x k))))))]))])
     helper)))
                               
(define sn-list-reverse-cps
  (cps-snlist-recur '() snoc-cps snoc-cps))

(define sn-list-occur-cps
  (lambda (s lst k)
    ((cps-snlist-recur 0 (lambda (a b k) (eq-cps a s (lambda (v) (if v (+-cps 1 b k) (+-cps 0 b k))))) +-cps) lst k)))

(define sn-list-depth-cps
  (cps-snlist-recur 1 (lambda (x y k) (apply-continuation k y)) (lambda (x y k) (max-cps (+ 1 x) y k))))                                                 

 
;------------------------------------------------------------------------------------------------------
;Problem 2
;------------------------------------------------------------------------------------------------------                   
(define memoize
  (lambda (pred hash equiv?)
    (let ([table (make-hashtable hash equiv?)])
      (lambda key
        (if (hashtable-contains? table key)
            (hashtable-ref table key #f)
            (let ([value (apply pred key)])
              (hashtable-set! table key value)
              value))))))


;------------------------------------------------------------------------------------------------------
;Problem 3
;------------------------------------------------------------------------------------------------------ 
(define  (subst-leftmost new old slist equality-pred?)
  (car (call-with-values 
    (lambda () 
      (trace-let loop ([slist slist])
        (cond 
          [(null? slist) (values '() #f)]
          [(and (symbol? (car slist)) (equality-pred? old (car slist)))
           (values (cons new (cdr slist)) #t)]
          [(symbol? (car slist))
           (let ([result (call-with-values (lambda () (loop (cdr slist))) list)])
             (values (cons (car slist) (car result)) (cadr result)))]
          [else 
            (let ([result (call-with-values (lambda () (loop (car slist))) list)])
              (cond
                [(cadr result) (values (cons (car result) (cdr slist)) #t)]
                [else (let ([result (call-with-values (lambda () (loop (cdr slist))) list)])
                        (values (cons (car slist) (car result)) (cadr result)))]))]
          )))
    list)))
  