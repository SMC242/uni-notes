---
tags:
- ADT/Tree

---
# Definition
A tree that can have a dynamic number of sub-trees. Accessing child nodes works by scanning the siblings until the desired node is reached

# Properties
- Accessing a child node is $O(k)$ where $k$ is the maximum [[maths/Trees/Tree#Degree|degree]] of all sub-branches
- This is due to searching the children from left-to-right 

# Members
Each node has:
- `key`: the value of the node
- `p`: the parent node
- `left_child`: the leftmost child of the current node
	- `left_child= NIL` means there are no children
- `right_sibling`: the next node to the right of the current node
	- `right_sibling = NIL` means there are no more siblings

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
`````