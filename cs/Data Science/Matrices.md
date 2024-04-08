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