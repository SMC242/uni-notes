---
tags:
  - Compression
---
# Overview
Huffman encoding is a data compression algorithm used for lossless data compression. It is a variable-length prefix coding algorithm that assigns shorter codes to more frequent symbols and longer codes to less frequent symbols, which makes it particularly efficient for compressing data with non-uniform symbol frequencies, such as text or images.

# Properties
- [[Algorithm Analysis#Time complexities|Time complexity]]
	- Best case: $O(n \log n)$, where n is the number of unique symbols in the input.
	- Worst case: $O(n \log n)$, where n is the number of unique symbols in the input.
- Space complexity: $O(n)$, where n is the number of unique symbols in the input.
- [[Algorithm Strategies|Strategy type]]: Greedy algorithm.
- Stable? Yes.
- Recursive? No, Huffman encoding is typically implemented iteratively using a [[priority queue]].
## Prefix property
- No codeword is a prefix of another
	- This is due to using [[trie]]s
- Avoids ambiguity about what the codeword should be decompressed to

# Use case
Huffman encoding is useful in scenarios where you want to compress data while preserving all of the original information. Common use cases include:
- Data transmission: Reducing the size of data for efficient transmission over networks.
- File compression: Reducing the size of files on disk to save storage space.
- Text compression: Compressing text files, e.g., in ZIP or GZIP formats.
- Image compression: Used in image formats like JPEG.

# Vanilla implementation
The Huffman encoding algorithm involves the following sub-procedures:
1. Frequency Counting: Calculate the frequency of each symbol in the input data.
2. [[priority queue]]: Create a [[priority queue]] ([[min-heap]]) based on the symbol frequencies.
3. Build [[Huffman tree]]: Build a [[Huffman tree]] by repeatedly merging the two nodes with the lowest frequencies until there's only one node left, the root of the tree.
4. Assign Codes: Traverse the [[Huffman tree]] to assign binary codes to each symbol, with shorter codes for more frequent symbols and longer codes for less frequent symbols.
5. Encoding: Encode the input data using the generated Huffman codes.
6. Decoding: Decode the compressed data using the [[Huffman tree]].

See also:
- [GeeksForGeeks](https://www.geeksforgeeks.org/huffman-coding-greedy-algo-3/) for a code example.
- 

# Variants
Some variants and extensions of Huffman encoding exist, such as:
- Adaptive Huffman coding: Allows the tree to adapt dynamically as data is encoded or decoded, useful for streaming data.
- Run-Length Encoding (RLE) with Huffman: Combining RLE and Huffman encoding for improved compression of runs of repeated symbols.
- Burrows-Wheeler Transform (BWT) with Huffman: Combining the BWT with Huffman encoding, as seen in the BZIP2 compression algorithm.