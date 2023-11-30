---
tags:
  - Graphs
---
# Overview
These are algorithms for traversing and searching [[Maths Map#Graphs|graphs]]

See first: [[Maths Map#Graphs|graph theory]]

# Key information
- A traversal is considered to be efficient if it traverses the whole graph in $O(|V| + |E|)$ time

# Traversals
[[Spanning Tree]]s are useful for visualising the coverage of a traversal

## Depth-first
For each component:
1. Follow one path of unvisited vertices until it ends
2. Backtrack until the an unvisited vertex is found
3. Continue until there are no reachable unvisited vertices left

- Usually implemented with a stack
- [[Time Complexity]] if implemented with:
	- A stack: $(O|V| + |E|)$
	- An adjacency list: $O(|V| + |E|)$
	- An adjacency matrix: $O(n^2)$

> [!EXAMPLE]
> ![Depth-first search example](https://upload.wikimedia.org/wikipedia/commons/7/7f/Depth-First-Search.gif)

See also: [[Depth First Search]]

## Breadth first search
For each component:
1. Search as widely as possible
	 - Visit all vertices that are adjacent to the current vertex ("processing the current vertex")
	- Vertices are processed in the order they are visited

- Typically implemented with a queue
- [[Time Complexity]] if implemented with:
	- A queue: $O(|V| + |E|)$
	- An adjacency list: $O(|V| + |E|)$
	- An adjacency matrix: $O(n^2)$

> [!EXAMPLE]
> ![Breadth first search](https://upload.wikimedia.org/wikipedia/commons/5/5d/Breadth-First-Search-Algorithm.gif?20100504223639)

See also: [[Breadth First Search]] 

# Shortest paths
![[Shortest Path Algorithms]]

# Minimum spanning tree
![[Minimum Spanning Tree]]

# Topological ordering
![[Topological Ordering]]