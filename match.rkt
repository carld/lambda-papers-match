#lang racket

(define-syntax test
  (syntax-rules ()
     ((_ expr res)
      (if (eq? expr res)
          (display "pass")
          (display "fail")))))

(define (match pattern expression)
  #f)

(test
 (match '(A) '(A))
 '[() _])