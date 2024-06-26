# Overview
- A [[Vectors|vector]] of random variables can be modelled using a multivariate distribution
	- Similar to a [[Probability Distribution#Probability density function|probability density function]]
	- Their [[Probability Distribution#Probability functions|probability function]] integrates to 1, just like a PDF
		- $\int_{x \in \mathbb{R}^{n}}f_{X}(x) = 1$

See also:
- [[Continuous Distributions]]

# Multivariate uniform
- A distribution where each component has the same [[Probability Distribution#Probability density function|probability]]
	- $f_{X} (x_{i)}= f_{X}(x_j)$ for a 2D vector
- Forms a box in [[Vector Spaces|vector space]]
	- $\int_{x} f_{X} (x) = 1, x \in \text{a box}$
- Can be transformed with a [[Matrices|matrix]] and/or shifted by adding a [[Vectors|vector]]

![A 3D multivariate uniform distribution](https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcThJ0aLCKXhnhIfvbV0A7jmEW13MxSgG_78APNczkKUjQ&s)

# Multivariate normal
- A ball transformed by the [[Matrices#Covariance|covariance matrix]] $C$ and shifted by the [[Vector Operations#Mean|mean vector]]

# Joint and marginal
- The [[Joint Probability|joint probability density function]] is the joint probability of the whole vector taking a particular value
- The [[Joint Probability#Marginal probability|marginal probability density function]] is the density for a subset of the dimensions
- You can also get the [[Conditional Probability|conditional]] densities