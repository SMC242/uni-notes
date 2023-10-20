---
tags:
  - Vector
  - LinearAlgebra
---
# Overview
- A vector space is a set of vectors
	- For my Data Fundamentals class, we are only working with real numbers and usually vectors of 3 elements ($\mathbb{R}^3$)

## Notation
- $\mathbb{S}^n$ where $\mathbb{S}$ is a [[Domains|set]] and $n$ is the number of fields
- $(\mathbb{S}^{n}, \mathbb{S}^{n}) \rightarrow \mathbb{S}$ is a binary function that maps a pair of vectors to a scalar value

## Topological vs inner product
- [[Vector Operations#Norm|Norm]] operations work in *topological vector space*
	- This means that there is a concept of "closeness" between vectors
- [[Vector Operations#Inner product|Inner products]] work in *inner product space*
	- Distance between vectors is talked about in terms of the angle between vectors

# Curse of dimensionality
> As dimension increases, generalisation gets harder *exponentially*

Most applications of [[Vectors]] in [[Data Science Map|Data Science]] use vectors with an unthinkable number of dimensions. This causes a few problems:

- Data-points will concentrate around the corners of the hyperspace
- This means that visualisation methods like binned histograms don't work so well because most bins are empty

---
Storing data like this takes up a huge amount of storage

---
- Algorithms that work well for few dimensions often fail when many dimensions are present

## Sparseness
- This sparseness means that data-points are less likely to be similar to each other because there are more ways to differ
- In fact, with many dimensions, the $L_{2}$ between any two points is almost the same
	- This begins to happen at $d > 5$ and becomes completely crippling at $d = 20$
- The hypercube will have $2^d$ corners
- Drawing a line between points in the hypercube will always be on the edge of the hypercube