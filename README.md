# Pattern Matcher with Backtracking for Segment Matches

From "Scheme an Interpreter for Extended Lambda Calculus" by Gerald Jay Sussman and Guy Lewis Steele Jr.

[http://repository.readscheme.org/ftp/papers/ai-lab-pubs/AIM-349.pdf]

Page 10 describes a pattern matching approach that incorporates back tracking.

Veit and I implemented this in DrRacket.

We took an incremental, test driven approach, committing as we reached a stage.

## 1. A testing mechanism

We came up with a basic mechanism for testing an expression against an expected return value. Example usage:

```
(test
 (match '(A) '(A))   ; an expression
 '[() _])            ; the expected result
```

Our implementation of `test` was in the form of a macro that would expand into a conditional statement:

```
(define-syntax test
  (syntax-rules ()
     ((_ expr res)
      (if (equal? expr res)
          (printf "pass~%")
          (printf "fail~%")))))
```

Next we started implementing `match`.

## 2. Matching a literal

To begin with we want the following test case to pass:

```
(test
 (match '(A) '(A))
 '[() _])
```

And so we came up with an initial `match` function that would pass that test:

```
(define (match pattern expression)
  (if (null? pattern)
      (if (null? expression)
          '[() _]
          #f)
      (if (eq? (car pattern) (car expression))
           (match (cdr pattern) (cdr expression))
           #f)))
```

If the pattern is null (an empty list), compare to the expression. If the expression is null too, return `'[() _]'`, otherwise return false.

If the patten is not an empty list, compare the first element of the pattern list to the first element of the expression list, if they are equal call `match` again to compare the rest of the pattern with the rest of the expression, otherwise return false.

The summary is we compare each element of the pattern list with each element of the expression list, until the end. If the two lists are different false is returned.

In DrRacket on OSX, we ran what we have so run by pressing '&#8984; r' (command r on Mac).

```
Welcome to DrRacket, version 6.5 [3m].
Language: racket, with debugging; memory limit: 128 MB.
pass
pass
>
```

We could also use a terminal to run racket:

```
% racket match.rkt
pass
pass
```

## 3.
