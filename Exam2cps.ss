;Clay Chapman
;Exam 2

(define apply-continuation
  (lambda (k . args)
    (apply k args)))

(define subst-left-cps-2args
  (lambda (new old slist equality-pred? k)
    (letrec ([helper
               (lambda (slist k)
                 (cond
                   [(null? slist) (apply-continuation k '())]
                   []
                   [else ]
                   ))])
      (helper slist k))))
                   
                    
                    
                    
(define subst-left-cps
  (lambda (new old slist succeed fail)
    (let loop ([slist slist] [succeed succeed] [fail fail])
      (cond
       [(null? slist) (fail)]
       [(list? (car slist))
        (loop (car slist)
              (lambda (substituted-car)
                 (succeed (cons substituted-car (cdr slist))))
              (lambda ()
                 (loop (cdr slist)
                       (lambda (substituted-cdr)
                           (succeed (cons (car slist) substituted-cdr)))
                       fail)))]
       [else (if (eq? (car slist) old)
                 (succeed (cons new (cdr slist)))
                 (loop (cdr slist)
                       (lambda (substituted-cdr)
                           (succeed (cons (car slist) substituted-cdr)))
                       fail))]))))                    