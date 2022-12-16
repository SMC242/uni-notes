# Product rule
Given 2 sets with $m$ and $n$ elements, there are $m \times n$ pairs

This works for any number of sets: $k_{1} \cdot k_{2} \cdots k_n$

# Sum rule
Given two **disjoint** sets with $m$ and $n$ elements, there are $m + n$ options 

This works for any number of sets: $k_{1}+ k_{2} + ... + k_{n}$

# Non-disjoint sum
If given two **non-disjoint** sets with $m$ and $n$ elements
$$| A \cup B| = |A| + |B| - |A \cap B|$$

# Permutations
$P(n, r)$ outputs the number of possible *ordered* combinations of elements
$$P(n,r) = \frac{n!}{(n-r)!}$$
where $n$ is the number of elements and $r$ is the length of each subset ($r = 2$ for pairs, $r=3$ for triplets, etc)

In this function's eyes, $\{A, B\}$ is a different combination from $\{B,A\}$

## Indistinguishable elements
You may have multiple sets that have elements like "XXXXXXX" and "YYYYYY". These elements are indistinguishable within their set. The permutations of such sets would be:
$$n!/(n_{1}! \cdot n_{2}! \cdots n_{k}!)$$

# Combinations
$C(n,r)$ is the number of possible *unordered* combinations of elements
$$\frac{n!}{r!(n-r)!}$$
where $n$ is the number of elements and $r$ is the length of each subset ($r = 2$ for pairs, $r=3$ for triplets, etc)

## With repetitions
If elements can be repeated, use this formula instead:
$$\frac{(n+r-1)!}{r!(n-1)!}$$