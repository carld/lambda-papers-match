#lang racket

(define-syntax test
  (syntax-rules ()
     ((_ expr res)
      (if (eq? expr res)
          (display "pass")
          (display "fail")))))

(define (match pattern expression)
  (if (null? pattern)
      '[() _]
       (if (eq? (car pattern) (car expression))
           (match (cdr pattern) (cdr expression))
           #f)))

(display (eq? '[() _] '[() _]))

(test
 (match '(A) '(A))
 '[() _])