# Overview
A sorting algorithm where a file is sorted in segments. This avoids loading the whole file into memory

See first:
- [[Storage Blocks]]

## Description
1. Split the file into $b/L$ blocks
	- $b$ is the number of [[Storage Blocks#Blocks|blocks]]
	- $L$ is the number of segments
2. Load each segment into memory and sort it using a [[Algorithmics Map#Sorting|sorting algorithm]] then store it again
3. Merge sorted sub-files in memory

# Properties
Data about the algorithm such as:
- [[Algorithm Analysis#Time complexities|Time complexity]]
	- Best case: $O(n \log n)$
	- Worst case: $O(n \log n)$
- Space complexity: $O(k)$
	- $k$ is the size of the segments
- [[Algorithm Strategies|Strategy type]]: Divide and conquer
- Stable? Sometimes
- Recursive?  If so, [[Recursion|what type]]?

# Use case
- [[Query Processing|Ad-hoc sorting]] in database queries

# Vanilla implementation
Explain the sub-procedures

See [Link to code implementation](https://example.com) for a code example

# Variants
Some algorithms have specialisations for specific problems