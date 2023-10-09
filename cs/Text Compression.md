---
"tags:": Compression
---
# Overview
Text compression aims to reduce the size of a text file

# Metrics
## Compression ratio
The size of the compressed file vs the original
$$
\begin{align*}
&\frac{x}{y}\\\\
\textrm{ where }\\
x &= \textrm{size of compressed file}\\
 y &= 
\end{align*}
$$

## Percentage saved
The percentage of space saved after compression. 40 - 60% is considered good

$$
\left(1 - \frac{x}{y}\right) \times 100
$$

## Weighted path length
- A measurement of the maximum path lengths in a [[Huffman Tree]]
- Lower is better
	- This means the average path is shorter --> traversals require less steps --> traversals are faster

# Algorithms
## Huffman encoding
- Statistical method: more common symbols get shorter encodings --> greater savings
- There are multiple [[Huffman Tree]]s for each input, but all of them have the same [[#Weighted path length|WPL]]

See:
- [[Huffman Encoding]]
	- [[Huffman Tree]]
- [[Huffman Decoding]]

## LZW compression
- Dictionary method: a mapping of strings to codewords
	- The dictionary is built dynamically
		- This means that most implementations only do one pass through the file
	- For each string `s`, every prefix of `s` will be present in the dictionary because it uses a [[Trie]]
- Used in [compress](https://linux.die.net/man/1/compress) (.z), [gzip](https://linux.die.net/man/1/gzip) (.gz), .gif and .tiff files

See:
- [[LZW Compression]]
- [[LZW Decompression]]

## Dictionary
- There are $2^k$ codewords available in the dictionary
	- $k$ is incremented when the codeword space is exhausted (doubles the available space)
	- The initial value of $k$ is arbitrary. 8 is typically chosen because it can represent all ASCII characters --> all single-letter strings in ASCII can be represented

## Decompression
- Happens one step out of phase with compression
- If a codeword `s` isn't in the dictionary yet, add `s + s[0]` to the dictionary