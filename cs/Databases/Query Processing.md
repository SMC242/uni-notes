# Overview
There are multiple strategies for performing each [[Fundamental operations|fundamental operation]]. Before running a query, the database engine evaluates the [[Database Files#I/O access cost|cost]] of each strategy and picks the most efficient option

See also:
- [[Query Optimisation]]

# Sorting
- Most queries involve ad-hoc sorting
	- E.G `SORT BY` and `GROUP BY`
	- `GROUP BY` is just sorting before clustering
- [[External Sorting]] is used because you can't load the entire relation into memory
- Expected cost of external sorting: $2b (1 + \log_{M}(L))$
	- $M$ is the degree of merging (number of sorted blocks merged per iteration)
	- $L$ is the number of sorted segments before merging
	- Meaning: a larger $M$ drastically reduces the blocks accessed (at the expense of memory usage)

# Select
1. Linear search over a [[Keys|key]]
	- $C() = \frac{b}{2}$
2. Binary search over key
	- [[Database Files#Heap files|Unsorted file]]: $\log_{2}(b) + 2b(1 + \log_M (L))$
	- [[Database Files#Ordered files|Sorted file]]: $\log_{2}(b)$
3. [[Indexing#Primary index|Primary index]] or hash function over a key
	- [[Indexing#Primary index|Primary index]]: $t + 1$ where $t$ is the [[Indexing#Multilevel Index|level of the index]]
	- [[Database Files#Hash file|Hash file]]: $1 + O(n)$ where $n$ is the number of overflow buckets
4. [[Indexing#Primary index|Primary index]] over key in a range query
	- [[Database Files#Ordered files|Sorted file]]: $t + O(b)$
	- You can't use [[Database Files#Hash file|hashing]] for range queries
5. [[Indexing#Clustering index|Clustering index]] over ordering non-key
	- $t + O(\frac{b}{n})$ where $n$ is the number of distinct values
	- Assumption: values are uniformly distributed
6. [[Indexing#Secondary index|Secondary index]] over non-ordering key
	- $t + 1$
7. [[Indexing#Secondary index|Secondary index]] over non-ordering, non-key
	- $t + m + O(b)$ where $m$ is the number of blocks with block pointers

## Logical selections
### Disjunctive selection
- Queries involving `OR`
- If all the attributes in the condition have an access path, use each access path to match against each condition
	- Take the [[Set Operations#Union|union]] of the resulting sets
- Otherwise, use a linear search 

> [!NOTE] Access paths
> Access paths are [[Database Files#Hash file|hashes]], [[Indexing#Primary index|primary indexes]], [[B+ Tree]]s, etc

### Conjunctive selection
- Queries involving `AND` 
- If there is an access path for an attribute, use it to match against the condition
	- The results are called "intermediate results"
- Filter the intermediate results by the other conditions in memory

> [!question] Which index should you use first?
> If you have multiple indexed attributes, use the one that will generate the smallest intermediate result set. Pray that it fits in memory
> 
> The number of tuples retrieved by a filter is called the "selectivity"

# Join
1. [[#Naive join]]: used when there is no access path
2. [[#Nested loop join]]: used when there is no access path
3. [[#Index-based nested-loop join]]: using an index or [[B+ Tree]]
4. [[#Merge-join]]: over a [[Database Files#Ordered files|sorted file]]
5. [[#Hash-join]]: over a [[Database Files#Hash file|hashed file]]

## Naive join
 1. Take the [[Set Operations#Cartesian product|Cartesian product]] of the two relations and write the result to a file
 2. Check if the attributes match for each tuple (I.E $R.A = S.B$ given $(r, s$). Store the matched tuples in another file

## Nested loop join
- Iterate over the blocks of each relation and see if they match
	- Multiple blocks are loaded at a time
- A buffer is used to store the matched tuples
	- If the buffer fills up, pause execution to dump it to the result file

```
FOR EACH r IN R DO       // The blocks of relation R
	FOR EACH s IN S DO  // The bocks of relation S
		IF r.A = s.B THEN
			Append(file, (r, s))
```

### Nested loop cost prediction
- If $n_{B}$ blocks are available in memory
	- 2 blocks are required for: reading the inner file, writing the result
	- ==> $n_{B}-2$ blocks are available for reading the outer file ("chunk size")
- Each block of the outer relation is read once
- The entire inner relation is read for each outer block
- Therefore, the database engine should put the bigger relation in the outer loop

> [!NOTE] Expected cost
> - Total blocks read for outer relation $E: n_{E}$
> - Total number of blocks in inner relation $D: n_D$
> - Number of chunks in the outer relation: $ceil(\frac{n_{E}}{n_{B}-2})$
> - Total number of blocks read in each outer loop iteration: $n_{D} \times ceil(\frac{n_{E}}{n_{B}-2})$
> 
> Using [[Query Optimisation#Selectivity|selectivity]] to refine this expression:
> - $jc = js \cdot |E| \cdot |D| = \frac{1}{\max(n_{E},m_{D})} \cdot |E| \cdot |D|$
> - Number of resulting blocks: $k = \frac{js \cdot |E| \cdot |D|}{f_{ED}}$
> 	- $f_{ED}$ is the blocking factor of the joined tuples (calculate by summing the record sizes and dividing by the block size)
> 	- This will be the number of blocks written to disk
> - Refined cost: $n_{D} + (ceil(n_{D}/(m - 2)) \cdot n_E  + (\frac{js \cdot |E| \cdot |D|}{f_{ED}}))$
> 	- $m$ is the number of blocks available in memory

## Index-based nested-loop join
- Use the index on one of the relations' attribute to get all matching tuples
	- E.G $I(R.A)$ to find all $s$ where $r.A = s.B$
- Faster than [[#Nested loop join]] because it avoids linearly searching the indexed relation
- Can't be used for recursive relationships

### Index-based cost prediction
$$n_{D} + r_{D} \times (x_{E} + 1)$$
Case: primary index (ordering, key field)
- $jc = js \cdot |E| \cdot |D| = \frac{1}{\max(n_{E},n_{D})} \cdot |E| \cdot |D|$
- Resulting blocks: $k = \frac{js \cdot |E| \cdot |D|}{f_{ED}}$
- Cost: $b_{E} + |E| \cdot (x_{D} + 1) \cdot (\frac{js \cdot |E| \cdot |D|}{f_{ED}})$

Case: clustering index (ordering, non-key) on $E.A$ with $x_E$ levels, selection cardinality $s_{E}$, blocking factor $f_{E}$
- $S_{E} = \frac{1}{NDV(A)} \cdot |E|$
- Blocks per cluster: $ceil(\frac{S_{E}}{f_{E}})$
- $k = \frac{js \cdot |E| \cdot |D|}{f_{ED}}$
- Cost: $n_{D} + |D| \cdot \left(x_{E} + ceil\left(\frac{s_{E}}{f_{E}}\right)\right) + \frac{js \cdot |E| \cdot |D|}{f_{ED}}$

Case: B+ tree on $A$ with $x_{E}$ levels, selection cardinality $s_{E}$, blocking factor $f_E$
- Selection cardinality $s_{E} = \frac{1}{NDV(A)} \cdot |E|$
- $k = \frac{js \cdot |E| \cdot |D|}{f_{ED}}$
- Cost: $n_{D} + |D| \cdot (x_{E} + y + s_{E}) + \frac{js \cdot |E| \cdot |D|}{f_{ED}}$
	- $y$ is the number of pointer-blocks
## Merge-join
AKA sort-merge

- Use [[Merge Sort]] over two sorted files
- Block-wise algorithm
- Cost: $n_{E} + n_{D} + \frac{js \cdot |E| \cdot |D|}{f_{ED}}$

Process:
1. Load a pair of blocks $\{R.block, S.block\}$
2. Linear scan both blocks concurrently using merge sort
3. Store matched tuples in a buffer

> [!NOTE] Unsorted files
> This strategy does work on [[Database Files#Heap files|unsorted files]], but will require ad-hoc sorting. This negates some of the performance gains 

## Hash-join
- Both relations must stored in a [[Database Files#Hash file|hash file]] using the **same** [[Hashing|hash function]]
	- They must both have the same number of buckets
- Has 2 phases:
	1. Partitioning: for each $r$, compute the bucket address of $r.A$ and put the tuple there in memory
	2. Probing
		1. For each $s$, compute the bucket address, put it there
			1. Compare each $r$ in the same bucket as $s$. Append matching pairs to the results file
- Cost: $3 (n_{E} + n_{D}) + \frac{js \cdot |E| \cdot |D|}{f_{ED}}$