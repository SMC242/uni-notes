---
tags: Graphs 
---
# Overview
A [[Directed Graph]] that is also [[Undirected Graph#Cycles|acyclic]]

# Sources and sinks
- Always has at least one source (start vertex) and at least one sink (end vertex)
	- This avoids cycles because otherwise you could keep adding vertices to the missing part of a path
- Sources have an [[Directed Graph#Degree|in-degree]] of 0
- Sinks have an [[Directed Graph#Degree|out-degree]] of 0