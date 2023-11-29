---
tags: Graphs 
---
# Overview
Depth-First Search (DFS) is a graph traversal algorithm that explores as far as possible along each branch before backtracking. It starts at a designated node and explores as deeply as possible along each branch before moving to the next branch. DFS can be implemented using both recursion and an explicit stack.

See first: [[Undirected Graph]]

# Properties
Data about the algorithm such as:
- [[Algorithm Analysis#Time complexities|Time complexity]]
	- Best case: $O(|V| + |E|)$, where $V$ is the number of vertices and $E$ is the number of edges.
	- Worst case: $O(|V| + |E|)$
- Space complexity: $O(V)$, where $V$ is the number of vertices. This accounts for the recursive call stack in the worst case.
- [[Algorithm Strategies|Strategy type]]: It is a searching algorithm and falls under the category of graph traversal algorithms.
- Stable? Yes, DFS is stable and deterministic.
- Recursive? Yes, DFS can be implemented recursively. It explores as far as possible along each branch before backtracking, which naturally fits a recursive structure.

# Use case
When is this algorithm useful?
- DFS is particularly useful for problems involving connectivity in graphs, such as finding connected components, detecting cycles, and pathfinding.
- It is commonly used in maze-solving problems, topological sorting, and solving puzzles with a branching factor.

# Vanilla implementation
DFS involves two main sub-procedures:
1. **Recursive DFS Function:**
   - Base Case: If the current node is the target or the graph is fully explored, return.
   - Mark the current node as visited.
   - Recursively call the DFS function on unvisited neighbors.

2. **Driver Function:**
   - Initialize an empty set to keep track of visited nodes.
   - Iterate through all nodes in the graph.
   - If a node is not visited, call the recursive DFS function on that node.

See [GeeksForGeeks DFS Implementation](https://www.geeksforgeeks.org/depth-first-search-or-dfs-for-a-graph/) for a code example.

# Variants
Some variants of DFS include:
- **Iterative DFS:** Uses an explicit stack to simulate the recursion, eliminating the risk of stack overflow for large graphs.
- **Randomized DFS:** Randomly selects the order in which neighbors are explored, useful in certain applications like random maze generation.
- **Bidirectional DFS:** Runs two simultaneous DFS searches, one from the start node and the other from the target node, meeting in the middle for more efficient pathfinding.