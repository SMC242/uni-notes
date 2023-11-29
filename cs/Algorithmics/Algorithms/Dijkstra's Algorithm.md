---
tags:
  - Graphs
---
# Overview
Dijkstra's algorithm is a graph search algorithm that finds the shortest path between two nodes in a weighted graph. It operates by maintaining a set of tentative distances to every node and iteratively selecting the node with the smallest tentative distance, updating the distances of its neighbors. Dijkstra's algorithm is widely used in routing and as a subroutine in other graph algorithms.

# Properties
Data about the algorithm such as:
- [[Algorithm Analysis#Time complexities|Time complexity]]
	- Best case: $O((V + E) \log V)$, where $V$ is the number of vertices and $E$ is the number of edges.
	- Worst case: $O((V + E) \log V)$
- Space complexity: $O(V)$, where $V$ is the number of vertices. This accounts for the priority queue or heap used to maintain tentative distances.
- [[Algorithm Strategies|Strategy type]]: Dijkstra's algorithm is a greedy algorithm, making the locally optimal choice at each stage.
- Stable? Yes, Dijkstra's algorithm is stable and deterministic.
- Recursive? No, Dijkstra's algorithm is typically implemented using an iterative approach with a priority queue.

# Use case
When is this algorithm useful?
- Dijkstra's algorithm is useful for finding the shortest paths between nodes in a graph with non-negative edge weights.
- It is commonly used in network routing protocols, such as OSPF (Open Shortest Path First) in computer networks.
- Applications include road network navigation systems and resource allocation in project management.

# Vanilla implementation
Dijkstra's algorithm involves three main sub-procedures:
1. **Initialization:**
   - Set the tentative distance of the start node to 0 and all other nodes to infinity.
   - Create a priority queue or heap and enqueue the start node with a priority of 0.

2. **Priority Queue-Based Iteration:**
   - While the priority queue is not empty, dequeue a node with the smallest tentative distance.
   - For each neighbor of the dequeued node, calculate the tentative distance through the current node and update if smaller.
   - Enqueue the neighbor with the updated tentative distance.

3. **Result Extraction:**
   - After the algorithm completes, the shortest path from the start node to any other node can be reconstructed from the tentative distances.

See [GeeksForGeeks Dijkstra's Algorithm](https://www.geeksforgeeks.org/dijkstras-shortest-path-algorithm-greedy-algo-7/) for a code example.

# Variants
Some variants of Dijkstra's algorithm include:
- **Bidirectional Dijkstra:** Runs two simultaneous Dijkstra searches, one from the start node and the other from the target node, meeting in the middle for more efficient pathfinding.
- **A* Algorithm:** An extension of Dijkstra's algorithm that incorporates a heuristic to prioritize paths likely to lead to the goal.