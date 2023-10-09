---
tags:
  - ADT/Trie
---
## Overview
- Implements [[Trie]]
- Each node represents a character from the keys.
- Nodes can have multiple children.
- Memory usage is optimized by collapsing nodes with single children into a single node.
- Each edge label represents a character on the path from the root to a child node.
- Nodes may contain associated values or simply serve as markers for key existence.

## Properties
## Time complexities of major operations:
- Insertion: $O(m)$, where $m$ is the length of the key to be inserted.
- Deletion: $O(m)$, where $m$ is the length of the key to be deleted.
- Search: $O(m)$, where $m$ is the length of the key to be searched.
- Prefix Search: $O(k)$, where $k$ is the length of the prefix.
  
## Upsides:
- Efficient for storing and searching keys with common prefixes.
- Space-efficient due to collapsing nodes.
- Well-suited for IP routing tables and text indexing.
  
##  Downsides:
- Requires more memory than other data structures like hash tables.
- Insertion and deletion can be slower for long keys compared to hash tables.

## Members
- Node Structure: Each node contains edge labels, pointers to child nodes, and optional values.
- Edge Labels: Represent characters on edges leading to child nodes.
- Pointer: Points to child nodes, optimizing memory usage.
- Value: Optional data associated with nodes.

- Extra members:
  - In some implementations, nodes may include additional metadata or flags to enhance functionality or support specific use cases.