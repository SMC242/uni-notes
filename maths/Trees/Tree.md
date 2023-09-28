---
tags: Trees

---
# Definition
A tree is a type of [[Graph]] with the following constraints:
- [[Graph#Direction|Undirected]]
- All nodes must be connected; there are no orphaned nodes
- [[Graph#Cycles|Acyclic]] (there are no cycles)

![Example tree](https://upload.wikimedia.org/wikipedia/commons/thumb/2/24/Tree_graph.svg/1200px-Tree_graph.svg.png)

# Nodes
- A node contains a value and, optionally, links ("edges") to other nodes
- A node that has no edges is called a "leaf node"

# Edges
![[Graph#Edges]]

# Paths
- A path is a sequence of nodes from one node to another
- The length of the path is the number of [[#edges]] on the path

# Size
- The number of nodes in a tree

# Degree
- The number of child-nodes of a nodee