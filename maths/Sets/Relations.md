# Overview
Relations are a set of 2-tuples representing some connection between the two elements

# Cross-product
$A \times B$ is the set of ordered pairs $(a,b)$ where $a \in A$ and $b \in B$

Example:
$$\begin{align*}\\
A &= \{a,b,c\}\\
B &= \{b,c,d,e\}\\
R&=\{(a,b),(a,e),(b,d),(c,c),(c,d)\}
\end{align*}$$

# Types
## Reflexive
- Every element is related to itself
- $(a,a)$ where $a \in A$

## Symmetric
- $a$ is related to $b$ when $b$ is also related to $a$
- If $(a,b)$ then $(b,a)$ where $a \in A, b \in B$

## Anti-symmetric
- $a$ is related to $b$ and distinct, but $b$ is not related to $a$
- $(a,b)$ and $a \neq b$, then $(b,a)$ is not a relation

## Transitive
- $a$ is related to $b$ and $b$ is related to $c$, therefore $a$ is related to $c$ (forms a triangle)
- If $(a,b)$ and $(b,c)$, then $(a,c)$

# Equivalence
- [[#Reflexive]], [[#Symmetric]], and [[#Transitive]] relations mean that their elements are equivalent

# Partial orders
- [[#Reflexive]], [[#Anti-symmetric]], [[#Transitive]] relations are partial orders
- This means that the elements are ordered, but some pairs of elements might not be related
- It can't have [[Undirected Graph#Cycles|cycles]]