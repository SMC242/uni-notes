---
tags:
  - Matrix
  - LinearAlgebra
---
# Overview
- Tables / 2D arrays
- They can be used as functions of [[Vectors]]
	- [[Matrix Operations#Multiplication|Multiplication]] by a matrix is like applying a function to a vector
- Two main abilities:
	- Rotation
	- Scaling
	- The combination of rotation and scaling is called "shearing"
- Can also be used as weightings
	- Common in [[Machine Learning]]

# Linearity
Only functions with this property can be written as matrices
- Straight lines remain straight
- Parallel lines remain parallel
- The origin doesn't move
- $A(x + y) = Ax + Ay$
- $A(cx) = cAx$

## Transforms
- Linear maps are functions $f: R^{m} \rightarrow R^{n}$ that maintain the linearity property
- If the map is $n \times n$, it will convert from the same vector space
	- Called a "linear transform"

## Projections
- If $Ax = AAx$, it's called a "linear projection"
- Applying it twice is the same as applying it once

# Definite matrices
- Matrices can be definite, semi-definite, or indefinite
	- Positive or negative

| Property               | Definition                                           |
|------------------------|------------------------------------------------------|
| Positive Definite      | All eigenvalues are greater than 0.                  |
| Negative Definite      | All eigenvalues are less than 0.                     |
| Positive Semi-definite| All eigenvalues are greater than or equal to 0.       |
| Negative Semi-definite| All eigenvalues are less than or equal to 0.          |
| Indefinite             | No other criteria met.                                |

![Graphed definite matrices](https://brickisland.net/cs177/wp-content/uploads/2011/11/ddg_definiteness.svg)

![Graphed definite matrices including positive](https://gregorygundersen.com/image/definiteness/definiteness.png)

> [!NOTE] Dot Products & Definitiveness
> - If the matrix is positive definite, $x^{T}Ax > 0$
> - This means that the [[Vector Operations#Inner product|dot product]] will be positive
> - $x$ won't be rotated by more than $90\degree$

# Linear systems
- A matrix can be written as weights (coefficients) of some variables, resembling a linear system of equations
- They can be solved using [[Matrix Operations#Inverse|inverses]] or [[Mathematical Optimisation]]
	- The inverse method [[Matrix Operations#Inverse|suffers from numerical instability]]

> [!EXAMPLE] Solution Via Inverse
> $$
> \begin{align*}
A&=\begin{bmatrix}\\
0.9 & 0.0 & 0.0 & 0.2 & 0.0 & 0.05 & 0.08 & 0.0
\end{bmatrix}\\
\text{Corresponds to }&\\
y_{1} &= 0.9x_1 + 0.2x_{4} + 0.05x_{6} + 0.08x_7\\
\\
&\text{To solve, left-multiply by the inverse}\\
Ax &= y\\
&= A^{-1}Ax\\
&= A^{-1} \frac{y}{x}\\
&= A^{-1} yx\\
&= A^{-1}y
\end{align*}
> $$

## Overdetermined systems
- A system is overdetermined if it has more inputs than required outputs
- They can be solved using a [[Matrix Operations#Pseudo-inverse|pseudo-inverse]]

$$A = X^+ Y$$
where $X,Y$ are input matrices

# Special matrices
## Diagonal
- Diagonal matrices have values along its diagonal and zeroes elsewhere
- Often used for computations because multiplying by them costs only $O(n)$ instead of $O(n^3)$

## Identity
- A square [[#diagonal]] matrix with 1s on the diagonal
- Has no effect when applied to another matrix or vector
- Denoted by $I$

## Zero
- A matrix of zeroes
- Collapses a matrix to the origin

## Orthogonal
- A matrix where the rows and columns are perpendicular to each other
- The vectors must be [[Vectors#Unit vectors|unit vectors]]
- Transforms a cube to a cube
	- Pure rotation, no scaling
- All of the [[#Eigenvalues|eigenvalues]] are $\pm 1$

> [!NOTE]
> They're referred to as "orthogonal" vectors instead of perpendicular vectors because orthogonality is the more general term. It encompasses all coordinate systems whereas perpendicularity is only defined in Euclidean geometry

## Covariance
- Denoted by $C$ or $\Sigma$
 - A matrix that represents the spread of the points in a matrix
 - Computes the variance of each vector from the [[Vector Operations#Mean|mean vector]]
 - The covariance ellipse can be plotted to show a circle that has been sheared to cover all the points
	 - Strongly correlated data will form an ellipse
	 - Weak or uncorrelated data will form a circle

### Inverse covariance matrix
- Use [[Matrix Operations#Singular value decomposition|SVD]] to get the singular values $\Sigma$, then $\frac{1}{\sqrt{\sigma}}$

## Square
- A matrix of shape $n \times n$
- Represents a transformation within a vector space $R^{n} \rightarrow R^n$
- Has many important properties
	- Inverse
	- Determinant
	- [[#Eigendcomposition]]

## Triangular
- A matrix with non-zero elements above or below the diagonal (including the diagonal)
- Either upper-triangular or lower-triangular

## Stochastic
- An [[Graph Algorithms|adjacency matrix]] for a [[Directed Graph]] where the total outgoing weight is 1
	- The sum of each row is 1
	- This means that the inputs and outputs of the graph are balanced
- Preserves mass under flow
- [[Vector Operations#Norms|L1 norm]] = 1

### Doubly stochastic
- Both the rows and columns sum to 1
- Flow is reserved in both directions
