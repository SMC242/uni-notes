---
tags: FileSystem
---
# Overview
Files are broken up into blocks. These blocks do not have to be contiguous in the storage device

- Multiple records form a block
- A file contains many blocks

# Records
There are two types of records:
- Fixed length
- Variable length

# Blocks
- Blocks are always fixed-length
- They contain a pointer (physical address) to the next block
	- Analogous to a [[Linked List]]

## Blocking factor
- The number of records per block
- In the context of [[Database Map|DBMSes]], the blocking factor is used to e

$$bfr = floor(B/R)$$
Where:
- $B =$ block size in bytes
- $R =$ record size in bytes
- $B \ge R$
	- There is at least one record in a block
