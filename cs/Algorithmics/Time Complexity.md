# Overview
- Time complexity is a mathematical function that describes how the amount of time it takes to run the function *grows*
- Do not confuse this with [[Running Time]]

# Cases
<dl>
	<dt>Best case</dt>
	<dd>The function is called with an input that makes it exit early</dd>
	<dt>Worst case</dt>
	<dd>The situation where the function runs through the entire input (E.G element not found when searching)</dd>
</dl>
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

# Growth rate
- The growth rate of a function is a property of the algorithm - it's always the same
- Having certain guarantees about the input can be used to write more efficient algorithms
	- a `maximum(xs)` function would take constant time if `xs` was always sorted in descending order (the biggest element is first)
	- This is how many algorithms work - they use an an appropriate data structure for the problem

# Example growth rates
- $O(1)$ constant time - some data structures offer functions like this
- $O(log n)$ - binary search
- $O(n)$ linear time - the holy grail of time complexities
- $O(n \log_2 n)$ logarithmic time - most sorting algorithms
- $O(n^2)$ quadratic time - getting into the danger zone
- $O(n^x)$ polynomial time - some matrix operations are $O(n^3)$
- $O(2^n)$ exponential time - not feasible
- $O(n!)$ factorial time - not feasible

![Time complexities graphed](https://miro.medium.com/max/1200/1*5ZLci3SuR0zM_QlZOADv8Q.jpeg)

# Simplifying growth rates
When talking about the growth rate of a function, ignore:
- Constant factors 
	- $+ 2$
	- $- 47^3$
- Lower-degree terms
	- $2n^{2} + 4n$ (quadratic function, ignore the linear term)
	- $3n^{3} + 2n^{2} + n$ (cubic function, ignore the other terms)

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