#lang racket

(define-syntax test
  (syntax-rules ()
     ((_ expr res)
      (if (equal? expr res)
          (printf "pass~%")
          (printf "fail~%")))))

(define (match pattern expression)
  (if (null? pattern)
      (if (null? expression)
          '[() _]
          #f)
       (if (eq? (car pattern) (car expression))
           (match (cdr pattern) (cdr expression))
           #f)))

(test
 (match '(A) '(A))
 '[() _])

(test
 (match '() '(A))
 #f)