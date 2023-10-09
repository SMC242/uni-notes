---
tags:
    - ADT/HuffmanTree
---
# Overview
- Which [[Abstract Data Type]] does this data structure implement?
    - This data structure implements a Huffman Tree, which is a specialized [[cs/Algorithmics/Data Structures/Trees/Binary Tree|Binary Tree]] used in data compression algorithms.
- What are its implementation details?
    - A Huffman Tree is a [[cs/Algorithmics/Data Structures/Trees/Binary Tree|Binary Tree]] where each leaf node represents a symbol (e.g., a character in text) and each non-leaf node represents the merging of two nodes. The tree is constructed in a way that minimizes the average encoding length for each symbol, making it efficient for data compression.

A Huffman Tree is an ADT that optimally encodes data for compression purposes.

![Huffman tree](https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/Huffman_tree_2.svg/1200px-Huffman_tree_2.svg.png)

# Properties
  [[Time Complexity|Time complexities]] of major operations:
- Construction (Building the Huffman Tree): $O(n \log n)$, where n is the number of unique symbols.
	 - Encoding: $O(n)$, where n is the length of the input data.
	- Decoding: $O(n)$, where n is the length of the compressed data.
  
  Upsides:
  - Efficient compression of data with variable symbol frequencies.
  - Lossless compression, meaning the original data can be perfectly reconstructed.
  Downsides:
  - Construction can be relatively slow for large datasets.
  - The compressed data may not always be smaller than the original data for all types of input.

# Members
- Implementation details about members:
    - Nodes: The tree consists of nodes, each representing either a symbol or a combination of symbols.
    - Parent-Child Relationships: Nodes are connected through parent-child relationships, forming the tree structure.
- Extra members:
    - Leaf Nodes: The leaf nodes of the tree represent individual symbols.
    - Internal Nodes: The internal nodes represent merged symbols.

# Operations
## Construction (Building the Huffman Tree)
- [[Time Complexity]]: $O(n \log n)$

```
Procedure BuildHuffmanTree(symbol_frequencies):
    Create a priority queue (min-heap) and add all symbols with their frequencies to it.
    While there is more than one node in the queue:
        Pop two nodes with the lowest frequencies.
        Create a new internal node with these nodes as children and a frequency equal to the sum of their frequencies.
        Add the new node back to the priority queue.
    The remaining node in the queue is the root of the Huffman Tree.
```

## Encoding
- [[Time Complexity]]: $O(n)$

```
Procedure Encode(data, huffman_tree):
    Initialize an empty bitstream.
    For each symbol in the data:
        Traverse the Huffman Tree to find the corresponding binary code for the symbol.
        Append the binary code to the bitstream.
    Return the encoded bitstream.
```

## Decoding
- [[Time Complexity]]: $O(n)$

```
Procedure Decode(encoded_data, huffman_tree):
    Initialize an empty string for the decoded data.
    Start at the root of the Huffman Tree.
    For each bit in the encoded data:
        Traverse the tree:
              - If the bit is '0', move to the left child.
              - If the bit is '1', move to the right child.
        When a leaf node is reached, append the corresponding symbol to the decoded data and return to the root.
    Return the decoded data.
```