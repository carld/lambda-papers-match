#lang racket

(define-syntax test
  (syntax-rules ()
    ((_ expr res)
     (if (equal? expr res)
         (printf "pass~%")
         (printf "fail~%")))))

(define (? s) (eq? #\? (string-ref (symbol->string s) 0)))

(define (match pattern expression)
  (define (matchfun p e res)
    (if (null? p)
        (if (null? e)
            `[,res _]
            #f)
        (cond
          ((? (car p))
           (if (hash-has-key? res (car p))
               (if (eq? (car e) (hash-ref res (car p)))
                   (matchfun (cdr p) (cdr e))
                   #f)
               (matchfun (cdr p) (cdr e) (hash-set res (car p) (car e)))))
          (else
           (if (eq? (car p) (car e))
               (matchfun (cdr p) (cdr e) res)
               #f)))))
    
    (matchfun pattern expression #hash()))
  
  (test
   (match '(A) '(A))
   '[#hash() _])
  
  (test
   (match '() '(A))
   #f)
  
  (test
   (match '(?A) '(B))
   '[#hash((?A . B)) _])