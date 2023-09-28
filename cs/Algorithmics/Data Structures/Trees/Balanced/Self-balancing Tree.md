---
tags:
- ADT/SelfBalancingTree

---
# Definition
A [[Binary Search Tree]] that minimises the [[Rooted Tree#Height|height]]. This solves the [[Binary Search Tree#Problems|issue]] of trees becoming unbalanced and degrading performance

# Motivation
## Use case
- Ensuring optimal performance of algorithms depending on [[Binary Search Tree]]s

# Implementations
All of implementations of this [[Abstract Data Type|ADT]]:

```dataview
LIST
FROM
	#ADT/SelfBalancingTree 
WHERE
	file.name != this.file.name
`````