# Experimental analysis
- Implementing the algorithm and [[Profiling]] it afterwards
<ul class="breakdown">
	<li class="pro">Easy</li>
	<li class="con">You have to implement and test it first, which may be slow</li>
	<li class="con">Results may not be representative of untested inputs</li>
	<li class="con">The environment (hardware and software) used to run the experiment affects the results; results are not comparable across environments</li>
</ul>

# Theoretical analysis
- Work out the computations based on [[#Analysing pseudocode|pseudocode]]
- Each operation (E.G indexing an array) takes some time which we assume to be constant
<ul class="breakdown">
	<li class="pro">You don't have to implement the algorithm</li>
	<li class="pro">Fast</li>
	<li class="pro">You can form a function of $n$ which describes the running time for all possible inputs</li>
	<li class="pro">Independent of the environment</li>
	<li class="con">BORING, VERY BORING. Math is for nerds</li>
</ul>

# Running time
![[Running Time]]

# Primitive Operations
- Expression evaluation
	- `1 + 1`
	- `x / y`
- Assigning to a variable
	- `x = 1`
	- `x = y`
- Indexing arrays
	- `xs[1]`
	- `xs[i]`
	- `m[i][j]`
- Function call
	- `f()`
	- `f(x)`
	- `instance.method()`
	- `String.join()`
- Returning from functions
	- Exiting the scope of a function

# Analysing pseudocode
- Count the [[#primitive operations]] on each line, then add them together and simplify

## Rules
## Loops
- The running time is all of the running time of all the statements in the body *multiplied by the number of iterations*

```
for i = 0 to n - 1 do
	x = x + 1
```
- The loop executes $n - 1$ times
- $O(n - 1)$

### Nested loops
- Nested loops multiply
```
for i = 0 to n - 1 do
	x = x + 1
	
	for j = 0 to n - 1 do
		y = y + 1
```
- $O((n - 1))^2)$

### Consecutive statements
- Add their running times
```
f(n) = [2 * x | x <- [0..n]]
y = 0     // 1 operation
z = f(5)  // O(5) operations
```
- $O(5)$

### If statements
$T(n) = T_1 + T_{2} + T_3$
	where
		$T_{1} =$ the running time of the condition
		$T_{2}=$ the running time of the first branch
		$T_{3} =$ the running time of the second branch
		
--> $T(n) = max(T_{1}, T_{2}, T_3)$

## Examples
- `x := arr[0]` is 2 operations: assignment and indexing
- `for i = 1 to n-1` is $2n$ operations: assign `i` and compare to `n - 1` for each element
- Loop bodies are usually executed $n - 1$ times

# Amortised analysis
![[Amortised analysis]]