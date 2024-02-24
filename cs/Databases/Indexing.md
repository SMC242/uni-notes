# Overview
Where [[Database Files|physical methods]] for efficiently storing data only support one field, indexing supports an unlimited number of sorting fields. This allows efficient queries over multiple fields

> [!NOTE] Silver bullet?
> Physical methods are not useless: they provide a *primary* access path whereas indexes are a *secondary* access path
> 
> Indexes are faster, but create a maintenance burden

# Properties
- Indexes are stored in separate files from the data
	- Smaller than the real data because they're just pointers and an index value
	- This provides a greater *density* (I.E greater [[Storage Blocks#Blocking factor|bfr]]):  there are more entries per block
- Each index applies to one field
- Index entries are:
	- Unique
	- Sorted
		- This means you can use a [[Binary Search]]
	- Fixed length
- Index entries are stored as a tuple of $(index \ value, block \ pointer)$

# Access process
1. Search the index for the value
2. Get the [[Storage Blocks#Blocks|block pointer]] from that index entry
3. Follow it to the [[Database Files|block in the database file]]
4. Load the block into memory

# Index density
- Dense index: one index entry per record
- Sparse index: only some records have an index entry

# Anchor records
- The first record in a block pointed to by an index entry

# Types

![Index type flow chart](https://editor.analyticsvidhya.com/uploads/5323310.PNG)

## Primary index
- When the index field is the ordering + key field of the corresponding [[Database Files#Ordered files|sequential file]]
- $(k_{i}, p_{i})$ where:
	- $k_{i}$ is the *unique* value of the index field
		- The key of the first record in the block - the [[#Anchor records|anchor record]]
	- $p_{i}$ is a pointer to a block containing the record with key = $k_{i}$
- One index entry per block
- Somewhat sparse

## Clustering index
- When the index field is the ordering field *but not* the key field of the corresponding [[Database Files#Ordered files|sequential file]]
- One cluster per unique value
	- Index entry = $(distinct\ value, pointer)$
	- The pointer is for the first block in the cluster
	- Subsequent blocks are linked with [[Database Files#Chain pointers|chain pointers]]
- Sparse

> [!NOTE] Search cost formulae 
> ### Equal query
> `SELECT k WHERE k = x`
> $$average \ cost = \log_{2}(m) + \frac{b}{n}$$
where:
> - $m$: number of distinct key values (I.E clusters)
> 
> ### Not equal query
> `SELECT k WHERE k != x`
> $$average \ cost = \frac{b(n + 1)}{2n}$$
> where:
> - $b$: number of blocks
> - $n$: number of records

### Efficiency
- Clustering indexes are efficient if $m \lt 2^{\frac{b(n + 1)}{2n}}$
- The cost of searching a clustering index is always at least as fast as $\frac{b}{2}$ block accesses
	- Formally: $lims_{n \rightarrow \infty} \frac{b(n + 1)}{2n} = \frac{b}{2} \lt b$

## Secondary index
- When the index field is a non-ordering field in a [[Database Files#Ordered files|sequential file]] or [[Database Files#Heap files|unordered file]]
- Dense
- There are two cases:
	- The index field is a [[#Key field|key]]
	- The index field is [[#Non-key field|not a key]]

### Key field
- Can't use [[#anchor records]] because the referenced file isn't sorted by the indexing field
- $(index \ value, pointer)$

### Non-key field
2 layers:
1. Cluster block addresses by distinct value
2. Create one index entry per cluster

![Non-key secondary indexing diagram](https://prepinstadotcom.s3.ap-south-1.amazonaws.com/wp-content/uploads/2023/01/Secondary-Indexing.webp)

## Multilevel Index
![[Multilevel Index#Overview]]