---
aliases: Eigenvectors
---
# Overview
- "Characteristic values"
- There are $n$ eigenvalues for an $n \times n$ matrix
	- These are unique to the matrix
	- The eigenvectors are not unique to the eigenvalue
- The eigenvalues are used in eigenvectors, which are special vectors that only get scaled (no rotation) when multiplied by the matrix
	- $Ax = \lambda x$ where $\lambda$ is the eigenvalue

See also:
- [[Matrix Operations#Eigendecomposition|Eigendecomposition]]

> [!NOTE] Applications
> - Finding the axes of rotation

> [!NOTE] Eigenspectrum
> - The eigenvalues of a matrix can be ordered according to their magnitude
> - Higher eigenvalues are more dominant
> 	- The transformation stretches more in these directions
> 	- If there is a large disparity in eigenvalues, the plot of the data will become a skinny ellipse after repeated applications of $A$
> - They are assigned ranks (0-based, lower is better)

# Algorithms
- [[#Power iteration]]
- [[Matrix Operations#Eigendecomposition|Eigendecomposition]]

## Power iteration
- A method for finding the leading eigenvector
- Repeatedly applying exponentiation and the $L_\infty$ norm
	- $X_{n} = \frac{Ax_{n-1}}{|Ax_{n-1}|_{\infty}}$