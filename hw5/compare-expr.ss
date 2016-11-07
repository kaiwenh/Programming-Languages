;check if there is built-in function on either side of the pair
(define (check-built-in-function x y)
  (or (equal? (car x) 'if)      (equal? (car y) 'if)
      (equal? (car x) 'let)     (equal? (car y) 'let)
      (equal? (car x) 'lambda)  (equal? (car y) 'lambda)
      (equal? (car x) 'quote)   (equal? (car y) 'quote)))


; build up the list in the form 'if TCP 'expr1 'expr2` 
(define buildlist
  (lambda (x y z w)
    (cons x (cons y (cons z (cons w '()))))))

;Compare two constant items: numeric, boolean constants or quoted lists
(define compare-constant
  (lambda (x y)
    (if (equal? x y)
      x
      (if (and (equal? x #t) (equal? y #f))
        'TCP
        (if (and (equal? x #f) (equal? y #t))
          '(not TCP)
          (buildlist 'if 'TCP x y))))))

; compare two equal-length lists: recursively compare each element of the two lists
(define compare-list
  (lambda (x y)
    (if (or (equal? x '()) (equal? y '()))
        '()
        (cons (compare-expr (car x) (car y)) 
              (compare-list (cdr x) (cdr y))))))

; Recursively check if the two 'let's bind the same list of variables
(define (compare-let-variables x y)
  (if (and (equal? x '()) (equal? y '()))
    #t
    (if (equal? (car (car x)) (car (car y)))
      (compare-let-variables (cdr x) (cdr y))
      #f)))

; if the 'let' expression binds the same variables, compare the body as a list
; otherwise compare them as constants
(define (compare-let x y)
  (if (compare-let-variables (car (cdr x)) (car (cdr y)))
    (compare-list x y)
    (compare-constant x y)))

; check if arguments in lambda are equal. if yes, then compare the body as a list
; otherwise treat them as constants and compare
(define (compare-lambda x y)
  (if (equal? (car (cdr x)) (car (cdr y)))
    (compare-list x y)
    (compare-constant x y)))


;compare-expr
(define (compare-expr x y)
  (if (and (list? x) (list? y)) ;check if 'a' and 'b' are lists
    (if (equal? (length x) (length y)) ;if yes, then check if they have the same length
      (if (equal? (car x) (car y)) ;if yes, then check if their first elements are the same
        (case (car x) ;if they are the same, then match them with the three cases:
          ('quote (compare-constant x y))
          ('lambda (compare-lambda x y))
          ('let (compare-let x y))
           (else (compare-list x y)))         
        (if (check-built-in-function x y) ;if not, check if either side has a built-in function
          (compare-constant x y);if yes => compare-constant
          (compare-list x y)));if no  => compare-list (ordinary list)
      (compare-constant x y));if no => compare-constant
    (compare-constant x y))); if no => compare-constant


; test-compare-expr
(define (test-compare-expr x y)
  (let ((result (compare-expr x y)))
    (and (equal? (eval (cons 'let (cons '((TCP #t)) (cons result '()))))
                 (eval x))
         (equal? (eval (cons 'let (cons '((TCP #f)) (cons result '())))) 
                 (eval y))))) 


; test-x
(define test-x
  '(cons
     ; Test 'let': different binding variables for 'let'
      (let ((d #t) (e 21) (f 31))
        ; Test 'lambda': different inputs, same body
        ((lambda (x y z) (if x y z)) d e f)) 
    (cons
      (let ((a 1) (b 2))
      ; Test 'lambda': same inputs, different bodies
        ((lambda (c d) (* c d)) a b))
      (cons
        ; Testing procedures, boolean constants
        (if (equal? (+ 2 2) 4) #t #f)
        (cons
          ; same lists
          '(if #t 1 2)
          ; different lists
          '(if #f 1 2))))))

; test-y
(define test-y
  '(cons
      (let ((a #t) (b 21) (c 31))
        ((lambda (x z y) (if x y z)) a b c))    
    (cons
      (let ((a 1) (b 2))
        ((lambda (c d) (* d c)) a b))
      
      (cons
        (if (equal? (- 5 1) 4) #f #t)
        (cons
          '(if #t 1 2)
          '(if #f 1 3))))))


;(compare-expr test-x test-y)
;(test-compare-expr test-x test-y)
