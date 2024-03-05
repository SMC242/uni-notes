# Overview
There are multiple strategies for performing each [[Fundamental operations|fundamental operation]]. Before running a query, the database engine evaluates the [[Database Files#I/O access cost|cost]] of each strategy and picks the most efficient option

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

