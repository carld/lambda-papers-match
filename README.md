# Pattern Matching with Backtracking for Segment Matches

From "Scheme an Interpreter for Extended Lambda Calculus" by Gerald Jay Sussman and Guy Lewis Steele Jr.

[http://repository.readscheme.org/ftp/papers/ai-lab-pubs/AIM-349.pdf]

Page 10 describes a pattern matching approach that incorporates back tracking.

Veit and I implemented this using DrRacket.

We took an incremental, test driven approach, committing to revision control (git)
as we reached each step.

## 1. A testing mechanism

We came up with a basic mechanism for testing an expression against an expected return value. Example usage of `test`:

```
(test
  (match
    '(A)    ; pattern argument
    '(A)    ; expression argument
  )
  '[() _]   ; the expected result
)
```

Our implementation of `test` came in the form of a macro that would expand into a conditional statement:

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

A literal match is where the pattern specifies exactly the value to match.

To begin with we want the following test case to pass:

```
(test        ; test macro
  (match     ; match function call
    '(A)     ; pattern argument
    '(A))    ; expression argument
  '[() _])   ; expected non false return value
```

*The expected return value `[() _]` is obtuse here. It is a list of two things, the first an empty list, the second, anything. Ultimately we want the first thing to contain names and values of matching expressions, the second to indicate a function that can be called to retrieve the next possible match where there may be more than one for the given pattern. For now consider the structure of this return value "for future use". It's not required for matching literals, which result in a true or false result.*

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

In DrRacket on OS X, we ran what we have so run by pressing '&#8984; r' (command r on Mac).

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

Next we implemented matching a single element of any value.

In the pattern this kind of match is represented by a symbol prefixed with a question mark `?`. It means match one element exactly one time, but it doesn't matter what the element is.

The symbol including the question mark becomes the name associated with the value that is matched.

Our test looked like this:

```
(test
  (match
    '(?A) '(B))
  '[#hash((?A . B)) _])
```

_In Racket, a hash map can be created with convenience syntax (e.g. `#hash((key . value))`)._

The pattern above says `?A` must match one element that can be anything. The return value of match must show `?A` and the value it matched. In this case `?A` matches one thing, and that thing is `B`.

The return value in the above test contains a hash with a key `?A` and value `B`.

First we came up with a predicate function to determine if a pattern element is prefixed with a question mark.

```
(define (? s)
  (eq?
    #\?
    (string-ref (symbol->string s) 0)))
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

There's a very important aspect to a non-literal match:
if the pattern contains more than one occurrence of the same named match,
all occurrences must have the same value consistently.

Another of looking at this aspect is through the following two test cases,
ask yourself which one will pass and which will fail?

```
(test
  (match
    '(?A ?A)             ; pattern with two occurrences of a named match
    '(B B))              ; expression
  '[#hash((?A . B)) _])

(test
  (match
    '(?A ?A)
    '(B C))
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

We had to look up a few hash functions in Racket documentation:
[hash-has-key?](https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._hash-has-key~3f%29%29),
[hash-ref](https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-ref%29%29),
[hash-set](https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-set%29%29)


## 4. Match a segment

The next and final stage is to implement a segment match.
A segment match means a match of zero or more elements.

The convention for this matcher is to prefix it with an exclamation mark `!`
(we called it a bang).
For example a matcher for zero or more elements in the pattern could
be `!A`.

We came up with a simple test:

```
(test
  (match
    '(!A)                       ; a named segment match
    '(B C))                     ; expression
  '[#hash((!A . (B C))) _] )    ; expected match result
```

Because a segment match is zero or more elements, `!A` should match the entire expression, `(B C)`.

First, we needed a predicate function to check for a `!` prefix.
It's more or less the same as the one we used for `?` prefixes.

```
  (define (! s)
    (eq?
      #\!
      (string-ref (symbol->string s) 0)))
```

Next we have to come up with a way to match zero or more elements.

This is where backtracking comes in.

A function is provided that will be called a match fails. This could be
because a literal constant failed to match, or either the pattern or expression
ran out of elements.

The initial implementation of the `cont` function just returns false:

```
(matchfun
  pattern
  expression
  #hash()
  (lambda () #f)    ; continuation function returns false
)
```

A new function, `matchn` is introduced to try matching n number of elements, that is, a segment of length n.

```
(letrec ((matchn (lambda (n)
            (if (>= (length e) n)
                (matchfun (cdr p)                           ; tail of the pattern
                          (drop e n)                        ; tail of the expression minus the first n elements
                          (hash-set res (car p) (take e n)) ; hash with head of pattern matched to first n elements of expression
                          (lambda () (matchn (+ n 1))))     ; attempt to match a longer segment of expression
                (cont)))))                                  ; continue, with previous shorter segment
  (matchn 0))
```

The `matchn` function attempts to match the number of elements provided
by it's single argument.

If the expression has as many or more elements than the number it's provided,
it carries on, calling `matchfun` with the named pattern assigned to that segment (length n) of expression.
It supplies a a continuation that will call `matchn` again with a longer segment.

Otherwise it will continue by calling `(cont)`,
which with either be a call to `matchn` with the shorter segment (effectively `n - 1`),
or the initial continuation which simply returns false.
