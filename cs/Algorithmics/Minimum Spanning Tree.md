---
tags: Graphs 
---
# Overview
![[Spanning Tree#Minimising weight]]

# Problem
Attempting to exhaustively search for the minimum spanning tree of a [[Undirected Graph#Connectivity|clique]] wouldn't work because there are $n^{n-2}$ spanning trees in a clique

# Prim-Jarnik
![[Prim-Jarnik#Vanilla implementation]]

- The naive implementation is $O(n^3)$
- With Dijkstra's refinement: $O(n^2)$
	- Store the best tree value
	- When finding a new vertex, compare candidate edges against its best tree value
	- Update the best tree value after inserting the best candidate into the tree