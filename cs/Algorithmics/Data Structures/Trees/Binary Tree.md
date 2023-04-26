---
tags:
  - ADT/BinaryTree
---

# Definition
A type of [[Tree]] that has at most two child [[Tree#Nodes|nodes]]. 

# Motivation
## Use case
- Used in many algorithms
	- See [[Binary Search]], [[Heap Sort]]

# Comparison
- Comparison to relevant structures
- Advantages and disadvantages \[in comparison to relevant structures\]

<ul class="breakdown">
	<li class="pro">Pro</li>
	<li class="con">Con</li>
</ul>

# Members
- `p`: the parent node
	- The [[Tree#Root|root node]] is the only node where `p = NIL`
- `left`: the left node. May be `NIL`
- `right`: the right node. May be `NIL`

# Operations
The operations that are defined for this structure and what they do

## Example operation
- Description


# Optional operations
These operations do not need to be implemented

# Implementations
All of implementations of this [[Abstract Data Type|ADT]]:

> [!WARNING]
> Edit the tag in this query
```dataview
LIST
FROM
	#ADT/NAME-OF-ADT 
WHERE
	file.name != this.file.name
```