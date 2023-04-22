# Overview
Asymptotic notations are used to describe the limits of a function's growth

# Big-Oh Notation
$O(f)$ where `f` is a function that takes $n$, the length of the input, as a parameter

## Formal definition
$f(n)$ is $O(g(n))$ if there exists some constants $c$ and $n_0$ such that $f(n) \le c \cdot g(n)$ for $n \ge n_0$

This notation gives an upper bound for the growth rate of a function
- $f(n) = O(g(n)) \rightarrow$ the growth rate of $f(n)$ is less than the growth rate of $g(n)$

### Bounds
- The running time is limited by $c \cdot g(n)$
- $n$ is unbounded
- This means that Big-Oh notation expresses the asymptotic upper bounds (the upper bound that is tended towards but never reached)
![Definition graph](https://cdn.programiz.com/sites/tutorial2program/files/big0.png)

# Big-Oh proofs
- $f(n) \le cn$
- Rearrange the equation to put $c$ and $n$ on different sides
- Pick values for $c$ and $n_0$ that satisfy the inequality

## Examples
## Prove true
$f(n) = 2n + 10$ is $O(n)$

$2n + 10 \le cn$
$2n - cn \le -10$
$cn - 2n \ge 10$ (Flip with$\times -1$)
$n(c - 2) \ge 10$
$n \ge \frac{10}{c - 1}$

Pick $c = 3, n_{0} = 10$

## Prove false
$f(n) = n^2$ is not $O(n)$

$n^{2}\le cn$
$n \le c$

$c$ must be a constant, therefore the expression does not hold for all $n$

# Other notations
![Graphed functions](https://www.dotnetlovers.com/images/coolnikhilj2256c883d1-b9fc-46e9-b225-588ac5063c3d.png)

## Big-Omega
- The lower bound of $f(n)$

$f(n) = \Omega(g(n))$ if there exists some $c, n_0$ such that $f(n) >= c \cdot g(n)$ when `n >= n_0`

## Big-Theta
- The upper and lower bounds of $f(n)$

$f(n) = \Theta(g(n))$ if and only if $f(n) = O(g(n))$ and $f(n) = \Omega(g(n))$