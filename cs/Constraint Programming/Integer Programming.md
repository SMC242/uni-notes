---
aliases:
  - ILP
  - Integer Linear Programming
---
# Overview
A type of [[Mathematical Optimisation|optimisation]] where all variables are [[Domains#Number sets|integers]]

Has a few sub-categories:
- Integer Linear Programming: dealing with linear constraints
- Mixed-integer Programming (MIP): some of the variables aren't integers
- Binary Integer Programming: the variables can either be 0 or 1

# Example: graph colouring
- For each vertex $i$, colour $j$
	- $c_{i,j} = 1$ if $i = j$, $0$ otherwise
		- Stores the chosen colour for node $i$
		- Read $c$ as "choices"
	- $w_{j} = 1$ if $j$ is used, 0 otherwise
- The [[Mathematical Optimisation#Objective function|objective function]] is $min \sum\limits_{j} w_{j}$
	- Minimises the number of colours
- The constraints are:
	- $\forall i \in V. \sum\limits_{i} c_{i,j} = 1$
		- There can only be one colour per node
	- $\forall (u, v) \in E, j \in C. x_{u,j} + x_{v,j} \le 1$
		- Each node's edges have different colours
	- $\forall i \in V, j \in C. x_{i,j} \le w_j$
		- Store the colours used in an array $w$ that counts up