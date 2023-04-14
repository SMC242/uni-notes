# Overview
- A function that describes how long a program takes to execute for a given input

# T-notation
$T(n)$ is the running time of for a given $n$
- $a$ is the time taken by the fastest [[Algorithm Analysis#Primitive Operations|primitive operation]]
- $b$ is the time taken by the slowest [[Algorithm Analysis#Primitive Operations|primitive operation]]

$a \cdot f \le T(n) \le b \cdot f$
	where $f(n)$ is the worst case [[time complexity]] of the function

# Bounds
The running time is bounded by the functions $a \cdot f$ and $b \cdot f$. This can be stated as *"$T(n)$ is bounded by two {[[Time Complexity#Growth rates|growth rate]]} functions"*

## Examples
- $f(n) = 8n - 3$: $T(n)$ is bounded by two linear functions
- $f(n) = n^{2} + 8$: $T(n)$ is bounded by two quadratic functions

# Does it change?
- Changing the environment affects $T(n)$ by a constant factor $c$
- $T(n)$'s growth rate does not change

![[Time Complexity]]

# Arithmetic on running times
Let $T_{1}(n) = O(f(n)), T_{2}(n) = O(g(n))$
# Addition
Adding two running times results in the maximum of the two operands

$T_{1}(n) + T_{2}(n) = O(max(f(n), g(n)))$

# Multiplication
Multiplying two running times results in the product of the two functions
- Practically, this means that nested loops multiply running times

$T_{1}(n) \times T_{2}(n) = O(f(n) \times g(n))$

## Proving linearity 
$T(n) = (\log_{2} n)^{k} \rightarrow T(n) = O(n)$