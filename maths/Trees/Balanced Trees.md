---
tags: Trees
---

# Definition
A type of [[maths/Trees/Binary Tree|binary tree]] where the height of the left and right sub-trees of a node have roughly the same [[Rooted Tree#Height|height]] ($\pm 1$ node). Used a lot in [[Algorithmics Map|Computing Science]]

![Example balanced tree](https://www.techiedelight.com/wp-content/uploads/Height-Balanced-Tree-2.png)

# Properties
- Height: $O(\log_2 n)$

See also: [[Asymptotic Notation|Big-Oh Notation]]

# Motivation
- Binary trees can be very unbalanced, leading to a large height (worst case: $O(n)$)
- This reduces the benefit of using a binary tree because traversing a poorly-balanced tree approaches $O(n)$ [[Time Complexity|complexity]]

# See also
- [[Self-balancing Tree]]