---
tags:
  - Vector
  - LinearAlgebra
aliases:
  - Vector
---
# Overview
Vectors are 1D arrays of a fixed length. They can be used to describe many things, such as points in a mesh, fields with direction ("arrows"), or points in $n$D space. In [[Data Science Map|Data Science]], they are thought of as points in space.

# Formal definition
- Ordered tuples 
	- May have a fixed number of fields ("finite-dimensional")
- All fields must be a member of the same [[Domains|set]]

# Vector spaces
![[Vector Spaces#Overview]]
![[Vector Spaces#Notation]]

# Types of vectors
## Unit vectors
- Vectors of length 1
- E.G $[1, 0, 0]$

> [!WARNING]
$[1, 0, 1]$ is not a unit vector because its length is 2

- Unit vectors almost always have a [[Vector Operations#Euclidean|Euclidean norm]] of 1
- Will always be on the unit sphere
![unit sphere](https://wiki.math.ntnu.no/_media/linearmethods/basicspaces/pnormunitsphere.jpg)

## Basis vectors
- Vectors from which all other vectors in the vector space can be formed

> [!NOTE] Remember
> For the vector space $\mathbb{R}^3$, $i$, $j$, and $k$ are the basis vectors
> 
> $$
\begin{align*}
i &= [1, 0, 0]\\
j &= [0, 1, 0]\\
k &= [0, 0, 1]
\end{align*}
$$

> [!EXAMPLE]
The other vectors in the set are just scaled versions of 
>
> $[5, 7, 3] = 5 \times [1, 0, 0] + 7 \times [0, 1, 0] + 3 \times [0, 0, 1]$

# Vector operations
![[Vector operations]]

# Feature vectors
- Data relevant to the model encoded as a vector

> [!EXAMPLE]
> When modelling flowers, the feature vector might look like this:
> `[petal size, number of petals, height, colour, lifespan]`
