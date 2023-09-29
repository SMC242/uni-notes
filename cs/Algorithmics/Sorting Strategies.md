# Comparison-based
 - Comparing two elements
	 - Creates a [[Decision Tree]]
 - Cannot be faster than $O(n \log n)$

## Proof
 - The decision tree property of this strategy means that the search space can only decrease at a logarithmic rate
	 - The number of leaf nodes (outcomes) $\ge$ the number of possible outcomes
	 - There must be at least as many outcomes as the number of possible orderings of `n` elements --> there are at least $n!$ leaf nodes
 - The worst case cannot be better than $O(h)$ where $h$ is the [[Rooted Tree#Height|height]] of the tree
	 - An execution of the algorithm is always a traversal from the root to a leaf node
 ![[maths/Trees/Binary Tree#Number of nodes|Binary Tree]]
 - Therefore, the limits of the number of leaf nodes are $n! \le 2^{h+1}-1 \le 2^{h+1}$ 
 $$
 \begin{aligned}
 2^{h+1} &\ge n! \\
 h + 1 &\ge \log_{2}(n!) & \mbox{log2 on both sides}\\
 &\gt \log_{2}(\frac{n}{2})^{\frac{n}{2}} & \textrm{since } n ! > \frac{n}{2}^{\frac{n}{2}} \\
 &= \frac{n}{2} \log_{2}\left(\frac{n}{2}\right)\\
 &= \frac{n}{2} \log_{2}(n) - \frac{n}{2} \log_{2}(2) \\
 &= \frac{n}{2} \log_{2}(n) - \frac{n}{2} \\
 &= O(n \log n) & \mbox{Simplify according to Big Oh rules}
 \end{aligned}
$$

# Structure-based sort
- Abusing a characteristic of the items being sorted

- [[Radix Sort]]