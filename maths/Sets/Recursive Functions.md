# Overview
Recursive functions are a category of [[Functions]] that are defined in terms of themselves. They are very similar to [[Induction Proofs]] as they use a base case and an inductive step

# Recursion
Base case: $f(0)$ or $f(\emptyset)$ or $f(1)$, etc
- Some identity value (the empty value or smallest value of a set)

Inductive step: $f(n) = f(n - 1)$
- The function is defined in terms of itself, repeatedly calling itself until it reaches a value it has been defined for
	- I.E the base case
- It then collapses the chain of function calls

## Call chains
Take the function $f(x) = f(x - 1) + x, f(0) = 0$
```
f(3) = f(2) + 3
f(2) = f(1) + 2
f(1) = f(0) + 1
f(0) = 0

Forms the chain:
(3 + f(2)) + (2 + f(1) + (1 + f(0)))

Evaluating the chain from the bottom up:
f(3) = 0 + 1 + 2 + 3
```
