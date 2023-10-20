# Overview
- Writing functions that can be run in parallel
- These functions are applied to the whole array
	- This means no explicit iteration
		- More readable
		- Iteration is abstracted away from the user
- Uses [[SIMD]] to process data in parallel

# GPUs
- Graphical Processing Units
- They are specialised for array computations
- Not good at doing logic; CPUs are better for this

# Vector spaces
- A vector in this field is a tuple with 3 elements

> [!EXAMPLE]
> $[5, 7, 3] = 5 \times [1, 0, 0] + 7 \times [0, 1, 0] + 3 \times [0, 0, 1]$

> [!NOTE] Remember
> $$
\begin{align*}
i &= [1, 0, 0]\\
j &= [0, 1, 0]\\
k &= [0, 0, 1]
\end{align*}
$$

# Vector operations
## Multiplication by scalar
$ax = [ax_{1}, ax_{2}, \dots ax_{n}]$

## Addition
AKA "vector composition"
$x + y = [x_{1} + y_{1}, x_{2} + y_{2}, \dots x_{d} + y_{d}]$

## Comparison
- Using [[#Norm]]s or [[#Inner product]]s

## Definition
For this field, we think of vectors as points in space. Other fields think about them as arrows

- Topological vs inner product