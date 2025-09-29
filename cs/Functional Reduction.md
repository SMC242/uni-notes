# Overview
Functional reduction is the process of reducing a complex series of expressions into a value.

```haskell
map (+1) [1, 2, 3]
 = map (+1) (1 : 2 : 3 : [])
 = 2 + (map (+1) (2 : 3 : []))
 = 2 + 3 + (map (+1) (3 : []))
...
 = 2 : 3 : 4 : []
 = [2, 3, 4] 
```

# Church-Rosser property
Expressions can be evaluated in any order. This means that expressions can be reordered

> [!EXAMPLE]
> `(10 * 4) + (5 * 3)` could have the LHS or the RHS solved first. It doesn't change the result