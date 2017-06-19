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

The expected return value `[() _]` is obtuse. It is a list of two things, the first an empty list, the second, anything. Ultimately we want the first thing to contain names and values of matching expressions, the second to indicate a function that can be called to retrieve the next possible match where there may be more than one for the given pattern. For now, consider this return value "for future use". These two not required for matching literals, which result in a true or false, `#f`, result.

We came up with an initial `match` function that would pass the test:

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

The summary is we compare each element of the pattern list with each element of the expression list, until the end. If the two lists are different false is returned. The end point is where there are no elements left in either the pattern list or the expression list, and `'[() _]'` is returned.

If the pattern is null (an empty list), compare to the expression. If the expression is null too, return `'[() _]'`, otherwise return false.

If the patten is not an empty list, compare the first element of the pattern list to the first element of the expression list, if they are equal call `match` again to compare the rest of the pattern with the rest of the expression, otherwise return false.

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

We also were careful to check that matching an empty pattern against a non-empty expression returned false.

## 3. Match a single element and return the value of that element

Next we implementing matching a single element of any value.
In the pattern this kind of match is represented by a symbol prefixed with a question mark `?`. It means match one element exactly one time, but it doesn't matter what the element is.

The symbol including the question mark becomes the name associated with the value that is matched.

Our test looked like this:

```
(test
   (match '(?A) '(B))
   '[#hash((?A . B)) _])
```

The pattern above says `?A` must match one element that can be anything. The return value of match must show `?A` and the value it matched. In this case `?A` matches one thing, and that thing is `B`.

First we came up with a predicate function to determine if a pattern element is prefixed with a question mark.

```
(define (? s) (eq? #\? (string-ref (symbol->string s) 0)))
```

We then added to our `match` function the condition to check to see if the current pattern element, that is the first element in the pattern, `(car p)`, is a single element matcher.

In addition we had to come up with a way to store the value of a matched element. We used a hash map to do this. The key of the hash map is the pattern element, i.e. `?A`, and the corresponding value is an element from the expression.

We had to introduce a new function, `matchfun`, that would receive a hash with a newly added key as single element matches occur.
It has three arguments:

* the pattern, `p`,
* the expression, `e`,
* and the hash, `res` (an abbreviation of result).

When the end of the pattern and expression is reached and there are no
elements left in `p` or `e` (they are both null),
`res` becomes the first element in the successful return value from `matchfun`.

There's a very important aspect to a non-literal match,
if our pattern contains more than one instance of this kind of match,
they should all represent the same value.

Another of looking at this aspect is through the following two test cases,
ask yourself which one will pass and which will fail?

```
(test
 (match '(?A ?A) '(B B))
 '[#hash((?A . B)) _])

(test
 (match '(?A ?A) '(B C))
 '[#hash((?A . B)) _])
```

If you said the first one will pass, and the second one will fail, you are right!

Because the pattern and expression are compared one element at a time from
left to right, we already have a value for the `?A` key in the hash on a
successive use of `?A` in the pattern. This means that later matches of `?A` must
agree with previously matched value of `?A`.

Our implementation of `match` now looked like:

```
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
```

## 4. Match a segment
