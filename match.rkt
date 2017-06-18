#lang racket

(define-syntax test
  (syntax-rules ()
    ((_ expr res)
     (let ((r expr))
       (if (equal? (if (list? r) (car r) r) res)
           (printf "pass~%")
           (printf "fail~%"))))))

(define (? s) (eq? #\? (string-ref (symbol->string s) 0)))
(define (! s) (eq? #\! (string-ref (symbol->string s) 0)))

(define (match pattern expression)
  (define (matchfun p e res cont)
    (cond
      ((null? p)
       (if (null? e)
           `[,res ,cont]
           (cont)))
      (else
       (cond
         ; one match, but can be anything (provided it can be unified with prior matches)
         ((? (car p))
          (if (> (length e) 0)
              (if (hash-has-key? res (car p))
                  (if (eq? (car e) (hash-ref res (car p)))
                      (matchfun (cdr p) (cdr e) res cont)
                      (cont))
                  (matchfun (cdr p) (cdr e) (hash-set res (car p) (car e)) cont))
              (cont)))
         ; zero or more matches (provided it can be unified with prior match)
         ((! (car p))
          (if (hash-has-key? res (car p))
              (let ((to-match (hash-ref res (car p))))
                (if (and (>= (length e) (length to-match)) (equal? to-match (take e (length to-match))))
                    (matchfun (cdr p) (drop e (length to-match)) res cont)
                    (cont)))
              (letrec ((matchn (lambda (n)
                                 (if (>= (length e) n)
                                     (matchfun (cdr p)
                                               (drop e n)
                                               (hash-set res (car p) (take e n))
                                               (lambda () (matchn (+ n 1))))
                                     (cont)))))
                (matchn 0))))
         ; a literal match
         (else
          (if (eq? (car p) (car e))
              (matchfun (cdr p) (cdr e) res cont)
              (cont)))))))
  
  (matchfun pattern expression #hash() (lambda () #f)))

(test
 (match '(A) '(A))
 #hash())

(test
 (match '() '(A))
 #f)

(test
 (match '(?A) '(B))
 #hash((?A . B)))

(test
 (match '(!A) '(B C))
 #hash((!A . (B C))))

(test
 (match '(!A !A) '(B C B C))
 #hash((!A . (B C))))