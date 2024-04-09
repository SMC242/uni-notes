---
tags:
  - Graphs
aliases: []
---
# Overview
An undirected graph is a set of vertices (nodes) with edges between them

- The set of vertices is finite
- The set of edges is infinite

![Undirected graph example](https://media.geeksforgeeks.org/wp-content/cdn-uploads/cycleGraph-300x156.png)

# Set representation
A graph can be represented as two sets:
- $V$, the set of all vertices in the graph
- $E$, the set of [[relations]] between members of $V$

# Adjacency
Two vertices are said to be adjacent if there exists an edge between the two
$$
\begin{align*}
V &= \{a,b\}\\
E &= \{ \{ a, z \} \}\\
&\therefore a \sim b
\end{align*}
$$

## Adjacency matrix
A [[Matrices|matrix]] consisting of boolean values (0 or 1) representing whether every vertex is adjacent to every other vertex
- Dimensions: $|a| \times |b|$

<img alt="Adjacency matrix example" height="400px" src="https://i.stack.imgur.com/GahiR.jpg" />

## Adjacency list
A list of all edges for each vertex
- Number of elements: $2|E|$

![Adjacency list example](https://www.oreilly.com/api/v2/epubs/9781788623872/files/assets/268857bd-bb32-4fa5-88c9-66d7787952e9.png)

# Incidence
When a vertex is involved in an edge, it is said to be incident to that edge

# Paths
- A path is a sequence of vertices
- Its length is the number of edges crossed

$a \rightarrow b \rightarrow c$

# Cycles
- A cycle is a path that returns to the starting element
- A graph is said to be acyclic if it has no cycles

$a \rightarrow b \rightarrow c \rightarrow a$

# Degree
The number of edges for a particular vertex

# Connectivity
There are a few types of connectivity

> [!NOTE] Connected
> All pairs of vertices are joined together by a path (there are no orphaned vertices)

> [!NOTE] Non-connected
> More than one connected components. A component is a system of connected vertices

> [!NOTE] Tree
> A connected, acyclic graph
>
> See also: [[maths/Trees/Tree|Trees]]

>[!NOTE] Forest
> - A graph containing only tree components
> - Acyclism is implied by only having trees

> [!NOTE] Complete/clique
> A graph where every pair of vertices is joined by an edge
>
> ![Clique example](https://media.geeksforgeeks.org/wp-content/uploads/complete-graphs-1.jpg)

> [!NOTE] Bipartite
> When a graph's vertices are in two [[Set Operations#Disjoint|disjoint sets]] 
>
> ![Bipartite graph example](https://upload.wikimedia.org/wikipedia/commons/b/b9/Simple_bipartite_graph%3B_two_layers.svg)
