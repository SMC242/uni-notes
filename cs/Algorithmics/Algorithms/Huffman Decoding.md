---
tags:
  - Compression
---
# Overview
Huffman decoding is the process of reversing the Huffman encoding algorithm to recover the original data from compressed data. It uses the [[Huffman tree]], which was used to encode the data, to decode it efficiently.

See first: [[Huffman Encoding]]

# Properties
- [[Algorithm Analysis#Time complexities|Time complexity]]
	- Best case: $O(n)$, where n is the number of bits in the compressed data.
	- Worst case: $O(n)$, where n is the number of bits in the compressed data.
- Space complexity: $O(1)$, as it typically decodes data on-the-fly without requiring additional memory.
- [[Algorithm Strategies|Strategy type]]: Tree traversal.
- Stable? Yes.
- Recursive? No, Huffman decoding is typically implemented iteratively using the [[Huffman tree]].

# Use case
Huffman decoding is used whenever you need to decompress data that has been encoded using the Huffman encoding algorithm. This includes scenarios such as:
- Decompressing files that were previously compressed using Huffman encoding.
- Reconstructing original text or images from Huffman-encoded data.

# Vanilla implementation
The Huffman decoding algorithm involves the following sub-procedures:
1. Build [[Huffman tree]]: Reconstruct the same [[Huffman tree]] that was used for encoding. This tree is crucial for decoding.
2. Decoding: Start at the root of the [[Huffman tree]] and traverse it based on the bits in the compressed data.
   - When encountering a '0' bit, move to the left child.
   - When encountering a '1' bit, move to the right child.
   - When you reach a leaf node (a symbol), output the corresponding symbol and return to the root of the tree to continue decoding.
3. Continue this process until you have decoded the entire compressed data.

The key to successful Huffman decoding is using the same [[Huffman tree]] that was used for encoding.

See [GeeksForGeeks]([https://example.com](https://www.geeksforgeeks.org/huffman-decoding/)) for a code example of Huffman decoding.

# Variants
There are no significant variants of Huffman decoding itself since it is a straightforward process based on the [[Huffman tree]]. However, variations in the Huffman encoding algorithm may affect the decoding process, such as adaptive Huffman coding, which dynamically updates the tree during encoding and decoding, but the decoding process remains conceptually similar.