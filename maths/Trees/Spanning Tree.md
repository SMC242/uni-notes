# Overview
A [[maths/Trees/Tree|Tree]] that includes all vertices in a graph
- A subgraph
- Created from a connected graph by deleting some edges

> [!EXAMPLE]
> ![Spanning tree](https://www.tutorialspoint.com/data_structures_algorithms/images/spanning_trees.jpg)

# Weight
If the original graph was [[Weighted Graph|weighted]], the weight of the spanning tree is the sum of its edges

# Minimising weight
The minimum weight spanning tree of a [[weighted graph]] represents the cheapest way of linking all the vertices

This is done by repeatedly deleting edges until every node has as few edges as possible without disconnecting any nodes