# Overview
A strategy for efficiently [[indexing]] databases

See first:
- [[Indexing]]
- [[maths/Trees/Tree|Tree]]

# How it works
- All index files:
	- Are ordered on the indexing field
	- The indexing field has a unique value
	- Index entries have a fixed length
- Therefore, index files can be [[Indexing#Primary index|primary indexed]]
	- The original index file is called the "base" or "level 1" index
		- [[Indexing#Index density|Sparse]]
	- Subsequent layers of indexing are called $level_{t}$ indexes
		- [[Indexing#Index density|Dense]]
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

## Proof

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

# Index tree
- Updating multi-level indexes is expensive
	- Each index is a file
	- Updates need to be reflected on every level
- Observation: multilevel indexes form a [[maths/Trees/Tree|tree]]
- Using a [[B-Tree]] allows the index tree to be dynamic
	- Self-balancing
	- Adjusts to deletions and insertions
	- Expands and contracts according to the distributions of values

> [!TIP] Tree diagram
> - The root is the top-most index
> - Non-leaf nodes are indexes
> - Leaves are [[Storage Blocks|data blocks]]
> 
> <img src="https://www.cs.uct.ac.za/mit_notes/database/htmls/media/chp11_11.png" alt="Index tree diagram" height="600px" />

## Specialised B-tree
Extends the normal B-tree node members
![[B-Tree#Members]]
- $Q_{i}$ points to the data-block holding $K_{i}$
- There are up to $p - 1$ keys and up to $p$ children per node
	- $p$ is the order of the tree (number of children per node)
- The child pointers are called "tree pointers"

### Redundancy
- If the tree goes over capacity, another layer is created
- This layer will be much larger than the last layer
- Most nodes are empty in this new layer (I.E redundant)
- Solution: [[B+ Tree]]s

> [!EXAMPLE]
> - $p = 16$
> - The root has 1 node, 16 pointers
> - Level 1: 16 nodes, 256 (16 * 16) pointers
> - Level 2: 256 nodes, 4096 (256 * 16) pointers
> - Level 3: 4096 nodes, null pointers
> - Adding level 4: 65,536 nodes, null pointers
> 	- Level 3 will be updated to have 65,536 pointers
