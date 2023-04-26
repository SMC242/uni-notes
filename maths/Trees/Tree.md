---
tags: trees 
---

# Definition
A tree is a type of [[Graph]] with the following constraints:
- [[Graph#Direction|Undirected]]
- All nodes must be connected; there are no orphaned nodes
- [[Graph#Cycles|Acyclic]] (there are no cycles)

# Nodes
- A node contains a value and, optionally, links ("edges") to other nodes
- A node that has no edges is called a "leaf node"

# Edges
![[Graph#Edges]]

# Paths
- A path is a sequence of nodes from one node to another
- The length of the path is the number of [[#edges]] on the path