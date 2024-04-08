---
tags:
  - Matrix
  - LinearAlgebra
---
# Multiplication
- $(R^{n \times m}, R^{m \times p} \rightarrow R^{m \times p}$
- The weighted sum
	- Element-wise multiplication of each row of $A$ by each column of $B$
- Not [[Properties|commutative]]
- $O(n^3)$

![Example](https://miro.medium.com/v2/resize:fit:1400/1*YGcMQSr0ge_DGn96WnEkZw.png)

# Exponentiation
- Multiply a matrix by itself
- Effect: repeatedly applies the operation represented by the matrix

$$A^{n} = \prod^{n}_{i=1} A$$

# Special matrices
## Diagonal
- Diagonal matrices have values along its diagonal and zeroes elsewhere
- Often used for computations because multiplying by them costs only $O(n)$ instead of $O(n^3)$

## Identity
- A square [[#diagonal]] matrix with 1s on the diagonal
- Has no effect when applied to another matrix or vector

## Zero
- A matrix of zeroes
- Collapses a matrix to the origin

## Covariance
 - A matrix that represents the spread of the points in a matrix
 - Computes the variance of each vector from the [[Vector Operations#Mean|mean vector]]
 - The covariance ellipse can be plotted to show a circle that has been sheared to cover all the points

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
- [[Vector Operations#Norms|L1 norm]] =1

## Doubly stochastic
- Both the rows and columns sum to 1
- Flow is reserved in both directions

## Eigenvalues/eigenvectors
- "Characteristic values"
- There are $n$ eigenvalues for an $n \times n$ matrix
	- These are unique to the matrix
	- The eigenvectors are not unique to the eigenvalue
- The eigenvalues are used in eigenvectors, which are special vectors that only get scaled (no rotation) when multiplied by the matrix
	- $Ax = \lambda x$ where $\lambda$ is the eigenvalue
	

> [!NOTE] Applications
> - Finding the axes of rotation

> [!NOTE] Eigenspectrum
> - The eigenvalues of a matrix can be ordered according to their magnitude
> - Higher eigenvalues are more dominant (E.G more important field in a study or more important postal office in a distribution network)
> - They are assigned ranks (0-based, lower is better)

### Power iteration
- A method for finding the leading eigenvector
- Repeatedly applying exponentiation and the $L_\infty$ norm
	- $X_{n} = \frac{Ax_{n-1}}{|Ax_{n-1}|_{\infty}}$

# Eigendecomposition
- Gets the [[#Eigenvalues/eigenvectors|eigenvectors]] for a matrix
- The vectors will be ranked (lower is more important)