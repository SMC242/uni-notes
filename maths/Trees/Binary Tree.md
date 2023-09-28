---
tags: Trees

---
# Definition
A type of [[Rooted Tree]] where each [[maths/Trees/Tree#Nodes|node]] may have at most two child nodes. These child nodes are referred to as left and right ($l, r$)

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
There are main three ways to traverse a tree. I will be working with the following Haskell definition in my implementations:
```haskell
data Tree a = Nil | Node a (Tree a) (Tree a)
```

>[!WARNING]
>It's worth reading [[Asymptotic Notation]] and [[Analysing Recursive Algorithms]] for the analysis sections. Read [[cs/Algorithmics/Data Structures/Trees/Binary Tree]] for information about the format of a tree node

## Preorder
- Output the element before exploring further
- Left biased
- The intuitive order

```
preorder(x)
	if x != NIL
		print x.key
		preorder(x.left)
		preorder(x.right)
```

```haskell
flattenPreOrder :: Tree a -> [a]
flattenPreOrder t = case t of
  Nil -> []
  Node x l r -> (x :) $ flattenPreOrder l ++ flattenPreOrder r
```

## Postorder
- Start outputting elements after exploring to the lowest level of the tree
- Left-biased

```haskell
flattenPostorder :: Tree a -> [a]
flattenPostorder t = case t of
	Nil -> []
	Node x l r -> flattenPostorder l ++ flattenPostorder r ++ [x]
```

## Inorder
Traversing with a left-bias - always tries to follow the left branch if it exists

```
inorder(x)
	if x != NIL
		INORDER(x.left)
		print x.key
		INORDER(x.right)
```

- Explore the left branches until the bottom is reached
- Start yielding outputs as the call chain collapses
- If a right branch is found, recur

```haskell
flattenInorder :: Tree a -> [a]
flattenInorder t = case t of
  Nil -> []
  Node x l r -> flattenInorder l ++ [x] ++ flattenInorder r
```

## Analysis
> [!INFO]
> The analysis is for the pseudocode because my Haskell solutions are inefficient

$T(n) = \Omega(n)$ because all nodes are visited

Using the [[Analysing Recursive Algorithms#Iteratively|iterative method]] to prove $T(n) = O(n)$:
$$
\begin{aligned}
& T(0) = O(1) \\
& T(n) = T(k) + T(n - k - 1) + O(1) & \mbox{right length = n - left length} \\
& T(0) + T(n - 0 - 1) + O(1) & \mbox{let k = 0} \\
& T(n - 1) + O(1) & \mbox{Substitute T(0)} \\
\\
& T(n - j) + jc_{2} = T(0) + nc_{2} & \mbox{j = n for base case} \\
&= c_{1} + nc_{2} \\
&= O(n)
\end{aligned}
$$
