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

## Multilevel index
- All index files:
	- Are ordered on the indexing field
	- The indexing field has a unique value
	- Index entries have a fixed length
- Therefore, index files can be [[#Primary index|primary indexed]]
	- The original index file is called the "base" or "level 1" index
		- Sparse
	- Subsequent layers of indexing are called $level_{t}$ indexes
		- Dense
- Strategy: split the indexes into a tree of $t$ levels
	- Going down one level drastically reduces the search space
- Optimise the number of levels using $t =\log_{m}(b)$
	- $m$ is called the "fan-out" or "index blocking factor"/$ibfr$
	- Works because $\log_{m}$ splits the search space into $m$ sub-spaces, reducing the number of steps drastically
- Trades off overhead for speed
	- $access \ cost = t + 1 = ceil(\log_{m}(b)) + 1$
- Used by all major DBMS systems

> [!NOTE] Number of blocks in an index
> $$b_{i} = ceil(b_{i - 1}/m)$$
> where:
> - $b_{i}$ is the number of blocks in the index at level $i$
> - $m$ is the fan-out (measured in blocks)

### Proof

$$
\begin{alignat*}{3}
&Let:\\
&B := block \ size \ in\  bytes\\
&r := number \ of \ records\\
&s := record \ size\\\\\\

&\textrm{File information}\\
&f = floor(B/s)\\
&b = ceil(\frac{r}{f})\\
&l := size \ of \ index \ entry\\\\

&\textrm{Level 1 index}\\
&m = floor(\frac{B}{l})\\
&b_{1} = ceil\left(\frac{b}{m}\right) & \mbox{b entries, b1 index blocks}\\\\

&\textrm{Level 2 index}\\
&b_{2} = ceil\left(\frac{b_1}{m^{2}}\right) & \frac{1}{m^{2}} \ less \ blocks \\\\

&\textrm{The level t index has 1 block}\\
&1 \le (\frac{b}{m^{t}})\\
&\Rightarrow t = \log_{m}(b)
\end{alignat*}
$$