#lang racket

(define-syntax test
  (syntax-rules ()
    ((_ expr res)
     (if (equal? expr res)
         (printf "pass~%")
         (printf "fail~%")))))

(define (? s) (eq? #\? (string-ref (symbol->string s) 0)))
(define (! s) (eq? #\! (string-ref (symbol->string s) 0)))

(define (match pattern expression)
  (define (matchfun p e res cont)
    (if (null? p)
        (if (null? e)
            `[,res _]
            (cont))
        (cond
          ((? (car p))
           (if (hash-has-key? res (car p))
               (if (eq? (car e) (hash-ref res (car p)))
                   (matchfun (cdr p) (cdr e) cont)
                   (cont))
               (matchfun (cdr p) (cdr e) (hash-set res (car p) (car e)) cont)))
          ((! (car p))
           (if (hash-has-key? res (car p))
               (let ((to-match (hash-ref res (car p))))
                 (if (equal? to-match (take e (length to-match)))
                     (matchfun (cdr p) (drop e (length to-match)) res cont)
                     (cont)))
               (letrec ((matchn (lambda (n)
                                  (matchfun (cdr p)
                                            (drop e n)
                                            (hash-set res (car p) (take e n))
                                            (lambda () (matchn (+ n 1)))))))
                 (matchn 0))))
          (else
           (if (eq? (car p) (car e))
               (matchfun (cdr p) (cdr e) res cont)
               (cont))))))
  
  (matchfun pattern expression #hash() (lambda () #f)))

(test
 (match '(A) '(A))
 '[#hash() _])

(test
 (match '() '(A))
 #f)

(test
 (match '(?A) '(B))
 '[#hash((?A . B)) _])

(test
 (match '(!A) '(B C))
 '[#hash((!A . (B C))) _])

(test
 (match '(!A !A) '(B C B C))
 '[#hash((!A . (B C))) _])