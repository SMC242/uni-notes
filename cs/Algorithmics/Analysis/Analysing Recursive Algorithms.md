# Recurrence equations
A recurrence equation describes the running time of a recursive algorithm. $T(1)$ is the base case and $T(n)$ is the recursive case. They're usually put into one equation like $T(\textrm{recursive case}) + T(\textrm{base case})$. 

## Method
- Get the [[running time]]s of the base and recursive case
- Put them into the form and simplify

### Examples
#### Factorial
```python
def factorial(n: float) -> float:
	if n == 1:  return 1           # T(1) = O(1)
	return n * factorial(n - 1)    # T(n) = T(n - 1) + O(1)
```

Overall equation: $T(n) = T(n - 1) + O(1)$

#### Merge sort
```python
def merge_sort(xs: list[T], lower: int, upper: int) -> None:
	if lower >= upper:  return  # T(1) = O(1)
	# T(n) = T(n / 2) + T(n / 2) + O(n)
	mid = (lower + upper) // 2
	merge_sort(xs, lower, mid)
	merge_sort(xs, mid + 1, upper)
	merge(xs, lower, mid, upper)
```

Overall equation: $2T(\frac{n}{2}) + O(n)$
See also: [[Merge Sort]]

## Solving
Recurrence equations can be solved to find their complexity in [[Asymptotic Notation#Big-Oh Notation|Big-Oh form]]

### Iteratively
- Replace constant terms with constants $c_n$
- Repeatedly substitute the recurrence relation until the base case is reached
	- Identify a value for $k$ such that the base case is reached (E.G $k = n - 1$)
- Substitute the running time of the base case for the final $T(n)$ term
	- Then replace with its constant $c_1$ (you can jump to this step)
- Substitute the equation for $k$

#### Examples
Using the `factorial` example:
$$
\begin{aligned}
T(1) &= O(1) \\
T(n) &= T(n - 1) + O(1) \\
\\

T(1) &= c_{1} \\
T(n) &= T(n - 1) + c_{2} \\
&= T(n - 2) + 2c_{2} \\
&= T(n - 3) + 3c_{3} \\
& ... \\
&= T(n - k) + kc_{2}  \\
&= T(1) + (n - 1)c_{2}  \\
&= c_{1} + (n - 1)c_{2}  \\
&= O(n)
\end{aligned}
$$

Using the `merge_sort` example:
$$
\begin{aligned}
T(1) &= C_{1}  \\
T(n) &= 2 T\left(\frac{n}{2}\right)+ c_{2}n \\
&= 4 T(\frac{n}{4})+ 2c_{2} n \\
&= 8 T\left(\frac{n}{8}\right)+ 3c_{2}n  \\
&= 16 T\left(\frac{n}{16}\right)+ 4c_{2}n  \\
& ... \\
&= 2^{k} T\left(\frac{n}{2^{k}}\right)+ kc_{2}n  \\
\\
& \textrm{Solving for k...} \\
& T\frac{n}{2^{k}} = T(1) \\
& \frac{n}{2^{k}} = 1 \\
& n = 2^{k} & \mbox{To exponent} \\
& \log n = \log 2^{k} & \mbox{Log both sides} \\
& \log n = k \log 2 & \mbox{Log rules}\\
& \log n = k \\
& 2^{\log n} = n \\
\\
& \textrm{Substituting equation for k...} \\
&= 2^{\log n}T(1) + (\log n \cdot c_{2}n) \\
&= c_{1}n + c_{2} \log n\\

&= O(n \log n)
\end{aligned}
$$

### Using a tree
- Draw the [[#Recursion tree]] with running times
- Calculate the running time for each level of the tree
- Sum the costs of each level

### Master theroem
- The fastest method
- Only applicable when the recurrence relation is of this form:

$$

T(n) = a T\left(\frac{n}{b}\right)+ f(n)
\textrm{ where }a \ge 1, b \gt 1
$$
There are 3 cases of $f(n) = \Theta(n^{c})$:
- $c \lt \log_{b}a$: $T(n) = \Theta(n^{log_{b}a})$
- $c = \log_{b}a$: $T(n) = \Theta(n^{c} \log n)$
- $c \gt \log_{b}a$: $T(n) = \Theta(f(n))$

See [[Asymptotic Notation#Other notations#Big-Theta|Big Theta Notation]] 

#### Examples
Using the `merge_sort` example:

- The recurrence relation is $T(n) = 2T\left(\frac{n}{2}\right)+ \Theta(n)$
- $a = 2, b = 2, f(n) = \Theta(n^1)$
--> case 2 because $c = 1$ and $\log_{b}a = log_{2}2 = 1$
$= \Theta(n^{c} \log n) = \Theta(n \log n)$

# Visualising recursive functions
## Recursion trace diagram
A recursion trace diagram can be used to show each step

![Recursion trace](https://d2vlcm61l7u1fs.cloudfront.net/media%2F752%2F7522d750-706b-4a02-aa6e-43acaafbdaff%2FphpM2PaE8.png)

## Recursion tree
- Representing the [[#Recursion trace diagram|trace]] of a recursive function is useful for [[#Binary|n-ary]] recursive functions

![Fibonnaci recursion tree](https://miro.medium.com/v2/resize:fit:925/1*svQ784qk1hvBE3iz7VGGgQ.jpeg)

# Analysing  recursion trees
- The height of a tree is the maximum number of edges from the root node to any node
	- I.E the number of layers
- The number of nodes in a binary tree is at most $2^{h + 1} - 1$ where $h$ is the height of the tree
- The Fibonacci function above is $O(2^n)$ because it is a binary recursive function of height $n$
	- Exponential complexity
