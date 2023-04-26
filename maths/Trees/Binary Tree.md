---
tags: Trees
---

# Definition
A type of [[Rooted Tree]] where each [[Tree#Nodes|node]] may have at most two child nodes. These child nodes are referred to as left and right ($l, r$)

![Example binary tree](https://static.javatpoint.com/tutorial/dms/images/discrete-mathematics-binary-trees.jpg)

# Usage
- Commonly used in [[Algorithmics Map|Computing Science]] because they half the search space each time you go down a level
	- This is how many algorithms achieve $O(\log n)$ [[Time Complexity|complexity]]

# Possibilities
The number of possible trees with $n$ nodes is given by:
$$
C_{n+1} = \sum\limits^{n}_{i=0} C_{i} \cdot C_{n-1}
$$
where
- $n >= 0$
- $C_{0} = 1$

or the [Catalan Number](https://en.wikipedia.org/wiki/Catalan_number) formula:
$$
C_{i} = \frac{2n}{(n + 1) \times \cdot n!}
$$

# Traversing
There are main three ways to traverse a tree:

## In order
```haskell

```