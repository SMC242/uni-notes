---
tags:
  - Graphs
---
# Overview
These are algorithms for finding the shortest path to a node in a graph

See first: [[Maths Map#Graphs|Graph theory]] - particularly [[Weighted Graph]]

# Dijkstra's algorithm
Let:
$$
\begin{align*}\\
s, v &= vertices\\
\pi &= \textrm{the shortest path from } s \textrm{ to } v\\
S &= \textrm{the set of vertices where the shortest path to } u \textrm{ is currently known}\\
d(v) &= \textrm{the shortest path between } u \textrm{ and } v\\
d(w) &= \textrm{the length of the shortest path between } u \textrm{ and } v \textrm{ passing through only vertices in } s
\end{align*}
$$

> [!EXAMPLE]
> ![Visualisation](https://i.makeagif.com/media/1-06-2021/1LMJQ-.gif)


See also: [[Dijkstra's Algorithm]]
## High-level process
- Find the shortest path between one vertex $u$ (the origin vertex) and all others
	- Initialise $S$ to $\{u\}$
- Each vertex is labelled with the length of the shortest path between $u$ and $v$, passing through only known vertices (vertices in $S$)
	- No path: $d(v) = \infty$
	- $v \in S \rightarrow d(v) = \min(d(u), d(v))$
	- This means that $v \in S \land w \notin S \rightarrow distance(u,w ) \ge distance(u, v)$
- After adding a vertex to $S$, update the length $d(w)$ for all vertices $\{w | w \notin S \}$
	- This is called [[#Edge Relaxation]]

## Edge relaxation
Recalculating the shortest paths after adding a new vertex to $S$

- If $v, w \notin S$:
	- The shortest path through $S = d(v)$ 
	- The shortest path between $u$ and $w$ through $S = d(w)$
- Calculate the shortest path between $u$ and $w$ through $S \cup \{v\}$ where $v$ is the new vertex
- The shortest path will be either:
	- The original path (length = $d(w)$)
	- The path through the new edge $e$ and the shortest path between $v$ and $u$ (length = $wt(e) + d(v)$)
	- I.E  $length = d(w) = \min(d(w), d(v) + wt(e))$
	- 