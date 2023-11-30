---
tags: Graphs 
---
# Overview
Topological ordering is an algorithm used in graph theory to linearly order the vertices of a directed acyclic graph (DAG) based on their dependencies. The ordering ensures that for every directed edge (u, v), vertex u comes before vertex v in the ordering. This concept is particularly useful in tasks like scheduling and task management.

# Properties
- **Time complexity:**
  - Best case: $O(V + E)$, where V is the number of vertices and E is the number of edges.
  - Worst case: $O(V + E)$
- **Space complexity:** $O(V)$, where V is the number of vertices.
- **Strategy type:** Topological ordering is based on a depth-first search (DFS) strategy.
- **Stable:** Yes, the order is deterministic and stable.
- **Recursive:** The standard implementation is recursive, utilizing depth-first search.

# Use case
Topological ordering is useful in scenarios where there are dependencies between tasks, and you need to find a linear order in which these tasks can be executed without violating any dependencies. Common applications include task scheduling, prerequisite resolution in course planning, and build systems.

# Vanilla implementation
The algorithm involves performing a depth-first search on the graph while maintaining a stack to keep track of the order in which vertices are visited. The recursive procedure explores the neighbors of a vertex and adds the vertex to the stack only after all its dependencies have been visited. The final order is obtained by popping elements from the stack.

**Link to code implementation:** [GeeksForGeeks Topological Sort](https://www.geeksforgeeks.org/topological-sorting/)

```python
def topological_sort_util(graph, vertex, visited, stack):
    visited[vertex] = True
    for neighbor in graph[vertex]:
        if not visited[neighbor]:
            topological_sort_util(graph, neighbor, visited, stack)
    stack.append(vertex)

def topological_sort(graph):
    visited = [False] * len(graph)
    stack = []
    
    for vertex in range(len(graph)):
        if not visited[vertex]:
            topological_sort_util(graph, vertex, visited, stack)
    
    return stack[::-1]
```

# Variants
Some variants of topological ordering include algorithms specifically tailored for certain types of graphs or constraints. For example, a lexicographic topological sorting can be used when multiple valid orderings are possible. Additionally, topological sorting can be extended to handle cyclic graphs with algorithms like Johnson's algorithm, which can find the order even when cycles are present.