;Clay Chapman
;Exam 1 part 2

;Problem 1
(define contains-both?
  (lambda (los sym1 sym2)
    (and (check-dups sym1 los) (check-dups sym2 los))))

;Problem 2
(define make-vec-iterator
  (lambda (v)
    (let ([vec v] [pos 0])
      (lambda (msg . args)
        (case msg
          [(val) (vector-ref vec pos)]
          [(set-val!) (vector-set! vec pos (car args))]
          [(next) (if (< (+ pos 1) 3) (set! pos (+ pos 1)))]
          [(prev) (if (>= (- pos 1) 0) (set! pos (- pos 1)))]
          [else (errorf 'stack "illegal message to stack object: ~a" msg)])))))
  
;Problem 3
(define matrix-sum
  (lambda (m1 m2)
    (cond
      [(or (null? m1) (null? m2)) '()]
      [else (append (list (helper1 (car m1) (car m2))) (matrix-sum (cdr m1) (cdr m2)))])))

;Creates new rows
(define helper1
  (lambda (r1 r2)
    (cond
      [(or (null? r1) (null? r2)) '()]
      [else (cons (helper2 (car r1) (car r2)) (helper1 (cdr r1) (cdr r2)))]
      )))

;adds elements from rows together
(define helper2
  (lambda (x y)
    (+ x y)))
  
;Problem 4
(define pascal-triangle
  (lambda (n)
    (cond
      [(< n 0) '()]
      [(= n 0) '((1))]
      [else (let ([triangle-n-1 (pascal-triangle (- n 1))])
              (cons (cons 1 (row-helper (car triangle-n-1))) triangle-n-1))])))

(define row-helper
  (lambda (prev-row)
     (map (lambda (x y) (+ x y)) prev-row (append (cdr prev-row) '(1)))))



;Helper Check-dups
        ;#t=duplicates exist, #f=no duplicates
(define check-dups
  (lambda (x y)
   (if (equal? y '())
        #f
       (if (equal? x (car y))
           #t
           (check-dups x (cdr y))))))

