---
aliases:
  - digraph
tags:
  - Graphs
---
# Overview
A graph where each edge has a direction

- Edges are represented as ordered pairs $(x, y)$
- The set of vertices and the set of edges are both finite

![Directed graph example](https://media.geeksforgeeks.org/wp-content/cdn-uploads/SCC1.png)

# Adjacency
Adjacency has a direction

> [!EXAMPLE]
> $a \rightarrow b$ 
> - $a$ is adjacent *to* $b$
> - $b$ is adjacent *from* $a$


## Adjacency matrix
Similar to [[Undirected Graph#Adjacency matrix|the adjacency matrix of an undirected graph]], except using the [[#Adjacency|definition of adjacency for directed graphs]]

> [!EXAMPLE]
> ![Example adjacency list](https://www.researchgate.net/publication/239491573/figure/fig2/AS:669390177591317@1536606463620/a-A-directed-graph-and-b-its-adjacency-matrix.ppm)

# Degree
The degree of a vertex is divided into in-degree and out-degree
- In-degree is the number of edges pointing to the vertex
- Out-degree is the number of edges pointing away from the vertex
