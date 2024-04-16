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
	- You can say "left-multiply" or "right-multiply" to be specific
- $O(n^3)$

![Example](https://miro.medium.com/v2/resize:fit:1400/1*YGcMQSr0ge_DGn96WnEkZw.png)

# Division
- Left-multiplication by the [[#inverse]]
- Undoes the transform

# Exponentiation
- Multiply a matrix by itself
- Effect: repeatedly applies the operation represented by the matrix

$$A^{n} = \prod^{n}_{i=1} A$$

# Decompositions
Matrix decompositions are algorithms for factorising matrices into its components

## Eigendecomposition
- Gets the [[#Eigenvalues]] for a matrix
- The vectors will be ranked (lower is more important)
- Useful for predicting behaviour across a time scale
	- E.G 7 days in the future
- The original matrix can be reconstructed from the eigenvalues and eigenvectors
- A matrix can be approximated using a some of the leading eigenvectors
	- Useful for large matrices

> [!WARNING] Constraints
> - Inaccurate on non-square matrices
> - The matrix has to be [[Matrices#Diagonal|diagonalisable]]

## Singular value decomposition
AKA SVD

$$A = U \Sigma V^T$$
where:
- $A$ is an $m \times n$ matrix
- $U$ is a square unitary matrix ($m \times m$)
	- The columns are left singular vectors
  - $V$ is another square unitary matrix $m \times m$
	  - The columns are right singular vectors
  - $\Sigma$ is a [[Matrices#Diagonal|diagonal]] $m \times n$ matrix containing the singular values
	  - These values are related to [[eigenvalues]], but not the same
	  - Always positive real numbers
	  - Pure scaling

- Factorises *any* matrix
	- In $O(n^3)$ time for [[Matrices#Square|square matrices]]
- If $A$ is real, $U,V$ will be [[Matrices#Orthogonal|orthogonal matrices]]. All rows and columns will have [[Vector Operations#Norms|norm]] = 1
- $U,V$ are [[Matrices#Orthogonal|orthogonal matrices]] and represent pure rotation
- Can be used to simplify a linear map
	- Technically, it finds a linear map that does a similar thing
	- Will have less flexibility

> [!TIP] Memorable Idiom
> The factors returned by the SVD can be remembered as "rotate-scale-rotate"


> [!NOTE] Special Case: Positive Semi-definite Matrix
> - The columns of $U,V$ will be the [[Eigenvalues|eigenvectors]]
> - $\Sigma$ will be the [[eigenvalues]]
>
> See also: [[Matrices#Definite matrices|Definite Matrices]]

### SVD and exponentiation
- $\Sigma$ can be used to compute fractional powers and [[#inverse]]s
	- Only if the matrix is [[Matrices#Square|square]] and symmetric
- $A^{n} = V \Sigma^{n}U^{T}$
- For inverses of non-symmetric matrices: $A^{-1} = V \Sigma^{-1}U^T$

> [!NOTE] Square Root
> The square root of a matrix can be computed from the SVD by computing the elementwise square root of $\Sigma$ and then substituting it into the aforementioned formula

### Rank
- The number of non-zero values in the matrix
- The rank is the number of dimensions in hyperspace that the transform represents
	- Measures the number of dimensions lost in the transform

| Condition                                                   | Rank                                          |
| ----------------------------------------------------------- | --------------------------------------------- |
| All non-zero singular values                                | Full rank                                     |
| Number of non-zero singular values $\ll$ size of the matrix | Low-rank matrix                               |
| Matrix not full-rank                                        | Singular (deficient rank), cannot be inverted |

### Condition number
- The ratio of the largest singular value (from $\Sigma$) to the smallest
- Only defined for full-rank matrices
- Indicates how sensitive the inverse of the matrix is to minor changes
	- Measures how close the matrix is to being [[#Determinant|singular]]

| Property               | Description      | Meaning                                                                                               |
| ---------------------- | ---------------- | ----------------------------------------------------------------------------------------------------- |
| Small condition number | Well-conditioned | Infrequent numerical issues                                                                           |
| Large condition number | Ill-conditioned  | Significant numerical issues (E.G [[Floating Point Representation#Round-off error\|round-off error]]) |

![[Condition Spectrum.png]]

# Principle component analysis
AKA PCA

- The [[#Eigenvalues|]] of the [[Matrices#Covariance|covariance]] matrix are known as the "principle components" of the matrix
	- They indicate the directions of greatest variance
- Length of a PC: $\sqrt{\lambda_i}$
- The direction of a PC is just the eigenvector

![PCA example](https://numxl.com/wp-content/uploads/principal-component-analysis-pca-featured.png)

> [!NOTE] Dimensionality reduction
> - The key use case for PCA is to reduce a dataset to fewer dimensions
> 	- To visualise it
> 	- To simplify a model
> 	- To apply algorithms that only work for low dimensions
> - Multiply the matrix by each PC to project the data to fewer dimensions
> 	- Pick the most important (longest) PCs and discard the others

# Trace
- The sum of the diagonal values of a matrix **or** the sum of its [[#Eigenvalues|eigenvalues]]
	- $Tr(A) = a_{1,1} + a_{2,2} + \dots + a_{n,n}$
	- $Tr(A) = \sum\limits^{n}_{i=1} \lambda_{i}$
- Only defined for square matrices

# Determinant
- Only defined for square matrices
- Measures the change in volume after the linear transform is applied
- Computed by multiplying the [[#Eigenvalues|eigenvalues]]
	- $det(A) = \prod\limits^{n}_{i=1}\lambda_i$
- If $det(A) = 0$, the transformation is lossy and can't be reversed
	- At least one dimension is collapsed
	- This is because at least one eigenvalue is 0
	- These matrices are called "singular"

# Inverse
- Only defined for square matrices
- Creates a new transform that undoes the original transform
- The [[#Singular value decomposition|SVD]] can be used to compute the inverse in some cases

> [!NOTE] Identities
> - $A^{-1}(Ax) = x$
> - $A^{-1}A = I$
> - $(A^{-1})^{-1} = A$
> - $(AB)^{-1} = B^{-1}A^{-1}$

> [!WARNING] Numerical stability
> Inverting matrices is numerically unstable with current algorithms. This is particularly bad with [[Floating Point Representation|floats]]

## Fast inversions
The following types of matrices can be inverted much quicker:
- [[Matrices#Orthogonal|Orthogonal]]
	- Rows and columns are orthogonal unit vectors
	- $A^{-1} = A^T$, so $O(1)$ time
- [[Matrices#Diagonal|Diagonal]]
	- $A^{-1} = \frac{1}{A}$
	- $O(n)$ time
- [[Matrices#Definite matrices|Positive definite]]
	- $O(n^2)$
- [[Matrices#Triangular|Triangular]]
	- $O(n^2)$

>[!WARNING] Sparse Matrices
> Inverting large sparse matrices is dangerous because the inverse will be dense and therefore cost a lot of memory

## Pseudo-inverse
- Approximates the inverse
- Works for non-square matrices
	- Implication: can solve [[Matrices#Linear systems|systems]] where the number of inputs and outputs is different
- Notation: $A^+$
- Built on top of the [[#Singular value decomposition|SVD]]
	- $A^{+} = V \Sigma^{-1}U^T$
	- $\Sigma$ needs to be zero-padded to be square

# Whitening
- The process of removing linear correlations before analysis
- $X^{W} = (X - \mu) C ^{\frac{-1}{2}}$
	- [[Vector Operations#Centring|Centres the data]]
	- Makes the distribution spherical by making the [[Matrices#Covariance|covariance]] = 1

> [!WARNING] Inverse of Covariance Matrix
> A special method is required to compute the inverse covariance matrix
> ![[Matrices#Inverse covariance matrix]]


See also:
- [[Matrices#Covariance|C, the covariance matrix]]
- [[Vector Operations#Mean|Mean vectors]]