---
tags:
- ADT/Tree

---
# Definition
A tree that can have up to $k$ sub-trees

# Properties
- You can have lots of children
- There will be lots of `NIL`s if most nodes don't fill all $k$ slots

# Members
Each node has:
- `key`: the value of the node
- `p`: the parent node
- $child_{i}..child_{k}$: pointers to sub-trees
s
