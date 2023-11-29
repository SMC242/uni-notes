---
tags: Graphs 
---
# Overview
Breadth-First Search (BFS) is a graph traversal algorithm that explores a graph level by level, visiting all neighbors of a node before moving on to the next level. It starts at a designated node and systematically explores its neighbors before progressing to their neighbors. Each vertex is visited exactly once

# Properties
Data about the algorithm such as:
- [[Algorithm Analysis#Time complexities|Time complexity]]
	- Best case: $O(|V| + |E|)$, where $V$ is the number of vertices and $E$ is the number of edges.
	- Worst case: $O(|V| + |E|)$
- Space complexity: $O(V)$, where $V$ is the number of vertices. This accounts for the queue used in the breadth-first traversal.
- [[Algorithm Strategies|Strategy type]]: It is a searching algorithm and falls under the category of graph traversal algorithms.
- Stable? Yes, BFS is stable and deterministic.
- Recursive? No, BFS is typically implemented using an iterative approach with a queue.

# Use case
When is this algorithm useful?
- BFS is useful for finding the shortest path in unweighted graphs, as it explores nodes level by level.
- It is commonly applied in network analysis, social network analysis, and shortest path problems when weights are uniform.

# Vanilla implementation
BFS involves two main sub-procedures:
1. **Queue-Based BFS:**
   - Enqueue the starting node and mark it as visited.
   - While the queue is not empty, dequeue a node, visit its neighbors, and enqueue unvisited neighbors.
   - Continue until the queue is empty.

2. **Driver Function:**
   - Initialize an empty set to keep track of visited nodes.
   - Enqueue the starting node.
   - While the queue is not empty, dequeue a node, mark it as visited, and enqueue unvisited neighbors.

See [GeeksForGeeks BFS Implementation](https://www.geeksforgeeks.org/breadth-first-search-or-bfs-for-a-graph/) for a code example.

# Variants
Some variants of BFS include:
- **Bidirectional BFS:** Runs two simultaneous BFS searches, one from the start node and the other from the target node, meeting in the middle for more efficient pathfinding.
- **Topological BFS:** Performs a BFS traversal while maintaining the topological ordering of nodes in a directed acyclic graph (DAG).