---
tags:
  - ADT/Tree
---

# Definition
In Computing Science, [[Rooted Tree]]s are usually used, particularly [[maths/Trees/Binary Tree|binary trees]]

# Motivation
## Use case
- Algorithms
- Hierarchical structures
	- File systems
	- Family trees

# Members
Each node has:
- `key`: the value of the node
- `p`: the parent node
- Some pointers to children - see [[#implementations]]

# Implementations
All of implementations of this [[Abstract Data Type|ADT]]:

```dataview
LIST
FROM
	#ADT/Tree
WHERE
	file.name != this.file.name
```

Some trees also balance themselves. See [[Self-balancing Tree]]