---
tags:
  - Vector
  - LinearAlgebra
---

# Overview
This is a list of operations that are defined for real vectors

# Scalar multiplication
- AKA weighting or scaling
- Multiply each element of the vector by a [[Tensor Types#Scalars|scalar]]

$$ax = [ax_{1}, ax_{2}, \dots ax_{n}]$$

# Addition
AKA "vector composition"

$$x + y = [x_{1} + y_{1}, x_{2} + y_{2}, \dots x_{d} + y_{d}]$$

# Weighted sum
- Addition and scalar multiplication at the same time
- The advantage of assigning a scalar factor for each value is that you can make some values more or less significant
- The vectors must have the same number of dimensions

$$[\lambda_{1} x_{1}, \lambda_{2} x_{2}, \lambda_{3} x_{3}]$$
where $\lambda$ terms are scalar values

## Linear interpolation
- AKA LERP
- Outputs vectors in a line between two vectors

$$lerp(x, y, \alpha) = (1 - \alpha)x + \alpha y$$
where:
- $x$ and $y$ are vectors
- $\alpha$ is the position along the line between $x$ and $y$ in the range $[0..1]$

# Comparison
- Performed using [[#Norm]]s or [[#Inner product]]s
- Which one you use depends on the type of distance you desire

![[Vector Spaces#Topological vs inner product]]

# Inner product
- AKA dot product
- Only works for vectors of the same dimension and they must be in [[Vectors#Topological vs inner product|inner product space]]

$$\cos \theta = \frac{x \cdot y}{||x|| ||y||}$$

> [!WARNING] Be careful...
> The output is in *radians*, not degrees

To reverse:
- Use $\arccos$

## Special cases
> [!NOTE] Unit vectors
> Since $||x||$ and $||y||$ will be $1$, $\cos \theta = x \cdot y$

> [!NOTE] For $\mathbb{R}^n$
> There is a simpler formula for vectors in the $\mathbb{R}^{n}$ space
> $\overrightarrow{x} \cdot \overrightarrow{y} = \sum\limits _{i} x_{i} y_{i}$


# Norm
- Norms are used to get the length of a vector
- Notation: $||x||_p$ where $p$ is the type of norm being applied
- $\mathbb{R}^{n} \rightarrow \mathbb{R}_{\ge 0}$
## General
The general form of a norm is:
$$||x||_{p} = \left(\sum \limits_{i} x^{p}_{i} \right)^{\frac{1}{p}}$$

## Euclidean
- AKA $L_2$ norm
- Gets the distance between elements (in the normal sense of the word)

$$||x||_{2} = \sqrt{x_{1}^{2} + x_{2}^{2} + x_{3}^{2} + \dots + x_{n}^2}$$

## Taxicab
- AKA Manhattan norm or $L_1$
- Gets the sum of the absolute values

$$||x||_{1} = \sum\limits^{n}_{r = 1} |x_{r}|$$

## L infinity
- AKA $L_{\infty}$ norm
- Gets the maximum element

$$||x||_{\infty} = max_{i} |x_{i}|$$

## Normalisation
- Making a vector have length 1 (I.E converting them to a [[Vectors#Unit vectors|unit vector]])
- Turns them into a unit vector signifying a direction

$$\frac{1}{||x||_{2}}$$

# Mean
- An average of a series of vectors
- Gets the geometric centroid of a set of vectors: the centre-point or "centre of mass" 

$$mean(\overrightarrow{x}_{1}, \overrightarrow{x}_{2}, \dots , \overrightarrow{x}_{n}) = \frac{1}{n} \sum\limits_{i} \overrightarrow{x}_{i}$$

## Centring
- You can move an array of vectors to $\mu= 0$ by subtracting the mean vector from each row

