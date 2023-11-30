---
tags: Graphs 
---
# Overview
The Prim-Jarnik algorithm, also known as Prim's algorithm, is a greedy algorithm used for finding the minimum spanning tree of a [[Undirected Graph#Connectivity|connected]], [[undirected graph]]. It starts with an arbitrary node and grows the spanning tree by adding the shortest edge that connects a vertex in the spanning tree to a vertex outside the tree.

# Properties
Data about the algorithm such as:
- [[Algorithm Analysis#Time complexities|Time complexity]]
	- Best case: $O((V + E) \log V)$
	- Worst case: $O((V + E) \log V)$
- Space complexity: $O(V + E)$
- [[Algorithm Strategies|Strategy type]]: Greedy
- Stable? Yes
- Recursive? No

# Use case
The Prim-Jarnik algorithm is particularly useful when the graph is sparse, meaning that there are relatively few edges compared to the number of vertices. It efficiently finds the minimum spanning tree in such scenarios.

# Vanilla implementation
The algorithm starts with an arbitrary node and repeatedly adds the shortest edge that connects a vertex in the spanning tree to a vertex outside the tree until all vertices are included. The process involves maintaining a priority queue to efficiently select the minimum edge at each step.

See [Link to code implementation](https://www.geeksforgeeks.org/prims-minimum-spanning-tree-mst-greedy-algo-5/) for a code example.

# Variants
Some variants of the Prim-Jarnik algorithm include optimizations for specific scenarios or additional constraints on the graph.
