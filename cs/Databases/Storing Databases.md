---
tags: FileSystem 
---
# Overview
The contents of a database must be stored in secondary storage. As reads to secondary storage are expensive, clever storage methods must be used to minimise latency

> [!NOTE] Types of storage
> There are three types of storage:
> 1. Primary: RAM, caches
> 2. Secondary: Hard drives (HDD), solid-state disks (SSD)
> 3. Tertiary storage: optical drives (disks inserted into the optical reader)
>
> Lower tiers trade off speed for storage capacity and reduced price

See first:
- [[Storage Blocks]]
- [[Predictions]] for the $E(x)$ notation

See also:
- [[Time Complexity]]

# I/O access cost
The time required for:
- Retrieving a [[Storage Blocks#Blocks|block]] ("search cost")
- Inserting, deleting, or updating a [[Storage Blocks#Records|records]] ("update cost")

> [!NOTE] Granularity of accesses
> You can't read/write single records from disk. You can only read/write whole blocks

## Cost function
A function $C$ that estimates the number of read/write operations for a search/insert/delete/update record operation for a value $X$

> [!EXAMPLE]
> $$
> \begin{align*}
> &values = \{ 1, 3, 6, 10 \}\\\\
> &C(X = 1) &= 3\\
> &C(X = 3) &= 1\\
> &C(X = 6) &= 2\\
> &C(X = 10) &= 4
\end{align*}
$$

## Expected access cost
$$E\left(C(X)\right) = \sum\limits_{x} P(X = x) \cdot C(x)$$
where:
- $C(X) =$ the cost function for accessing $x$ in block accesses

> [!EXAMPLE] Example continued
> The expected cost for the values from before is:
> $$
\begin{align*}
E \left(C(X) \right) &= \sum\limits_{x} P(X = x) \cdot C(x)\\
 &= 2.5\ block\ accesses
\end{align*}
$$

# Deleting records
- Set the deletion flag
- Periodically, the marked [[Storage Blocks#Records|records]] are actually deleted and the surviving records are reorganised into blocks

# Heap files
AKA unordered file

- An unordered file
- [[Storage Blocks#Records|Records]] are appended to the last block in the file

## Operations
### Insertion
- $O(1)$
- 2 operations
	- Load last block
	- Add record to end of block and write it

### Retrieval
- $O(b)$ where $b$ is the number of blocks
- $(b + 1) / 2$ operations
	- Load a block
	- Search block for record

### Deletion
- $O(b)$
- $b + 1$ operations
	- Retrieve block containing record
	- Remove record from block and write it

# Ordered files
AKA sequential file

- Records are physically sorted by a field ("ordering field")
- Good for the following query types:
	- Sequential scanning (`ORDER BY`)
	- Searching by the ordering field (`WHERE OrderingField = ...`)
	- Ranges over the ordering field (`WHERE OrderingField > 1 AND OrderingField < 8`)

## Chain pointers
- Storing the records as a sorted [[Linked List]] makes ordered files more efficient
- Each record holds a pointer to the next one
- Problem: the file can decay into an unsorted linked list

## Operations
### Insertion
- $O(\log_{2} b) + O(b)$
	- [[Binary Search]] for the block
	- Move blocks after the insertion point
		- **Half** of the blocks on average
- Using a [[Linked List]]:
	- When inserting, if the next block isn't free, put the record into an overflow block and point to it
	- The pointers must be updated

### Retrieval
- $O(\log_{2}b)$ if searching on the ordering field
	- Uses [[Binary Search]]
- $O(b)$ otherwise

#### Range queries
- $O(\log_{2} b) + O(b) = O(\log_{2}b)$
	- [[Binary Search]] until the first block meets the requirement
	- Consume blocks until out of range

### Deletion
Assuming a linked list is being used:
- $O(\log_{2} b) + O(1)$
	- [[Binary Search]] for the record
	- Set deletion flag
	- Update last record's pointer to not point to the deleted record
- The file has to be re-sorted periodically to prevent decay
	- Deletion is considered expensive because of this

### Update
- If updating on the ordering field: deletion + insertion (expensive)
- Otherwise: $O(\log_{2} b) + O(1)$

# Hash file
- A [[Hashing|hashing function]] is applied to a field in each record ("hashing field")
	- $y = h(k)$
- The output is the [[Storage Blocks#Blocks|block]] address to store the record in
- Essentially a [[Hash Map]]
	- Each block is a bucket

> [!NOTE] Even distribution
> The hash function should distribute evenly. I.E the probability that a key $k$ is in a bucket $m$ should be $1/M$
>
>Here is a hashing function that meets that requirement
> $$y = h(k) = k\mod M$$

## Operations
### Retrieval
- $O(1)$
	- Hash $k$
	- Find block address using $h(k)$
	- Linear search block until record is found

### Insertion
- Best case: $O(1)$ 
	- The record is in the first block
- Worst case: $O(n)$ where $n$ is the number of overflow blocks ($n \lt b$)
	- When a block fills up, it points to another block for subsequent insertions

### Deletion
- Best case: $O(1)$
- Worst case: $O(n)$ where $n$ is the number of overflow blocks ($n \lt b$)
	- Have to traverse the block pointers
- When records are cleaned up periodically, the blocks that lost records will be combined with other blocks of the same $y$ value

### Updating
- Best case: $O(1)$
- Worst case: $O(n)$
	- Have to traverse the block pointers
- 