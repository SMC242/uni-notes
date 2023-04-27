---
tags: ADT/SelfBalancingTree 
---

# Overview
A [[Self-balancing Tree]] that 

# Properties
- Good for big datasets on secondary storage devices (hard drives)
	- This data structure minimises I/O
- Can have more than two children
	- Potentially thousands
	- This ends up being faster because data is retrieved from secondary storage in large "pages" instead of single items
	- Minimising the number of nodes means more keys can be retrieved at once
- Height is $O(\log n)$, but provably a lot smaller than a [[Red-black Tree]]

## Constraints
1. Leaves can't have children
2. The keys represent the range of keys in their sub-trees
![[B-Tree.png]]
3. All leaves must be at the same depth - the tree height
	- I.E all layers end at the same depth
4. Each node must have at least $t - 1$ keys
5. Each node can have up to $2t - 1$ keys
	- The node is considered full when this bound is reached

# Members
Trees have:
- `root`: the root node
	- Always stored in memory
	- [[#disk-write]] will be called when the root changes
- `t`: the minimum [[maths/Trees/Tree#Degree|degree]] of a tree
	- $t \ge 2$

Nodes have:
- `n`: the number of keys in the node
- `key[0..n - 1]`: the keys stored in non-decreasing order (ascending order with duplicates allowed)
- `leaf`: whether the node is a leaf
- `c[0..n]`: the `n + 1` children of the node
	- Will be `NIL` when `leaf = true`

# Operations
## Disk-read
- Read a page from secondary storage

## Disk-write
- Write to the secondary storage

## Search
- Search for a key in the tree
- Use the number of children to inform where to search next
- $O(\log_{t} n)$ disk accesses
- Total CPU time: $O(t \log_{t} n)$

```python
def search(self: BTree[T], k: T) -> (Node[T], int) | None:
	def inner(x: Node[T]) -> (Node[T], int) | None:
		i = 0
		# Find the index of k < k_i
		while i < x.n and k > x.key[i]:
			i += 1
		if i < x.n and k == x.key[i]:
			return (x, i)
		elif x.leaf:  return None
		else:
			self.disk_read(x, i)  # Read the ith child of x
			return inner(x.c[i])
	return inner(self.root)
```

## Split-child
- Split a leaf into two nodes
- Move the middlemost key (median) to the leaf's parent node

### Detailed description
- The input node `x` must not be full
- The input index is for `y`, a full node that is a child of `x` (`x.c[i]`)
- `y` gets split at its median key, which is put into its parent, `x`
- `z` is the new node that holds all keys greater than the median
	- Becomes a child of `x`

```python
def split_child(self: BTree[T], x: Node[T], i: int) -> None:
	z = allocate_node()
	y = x.c[i]
	z.leaf = y.leaf
	z.n = self.t - 1
	for j in range(0, t - 2):
		z.key[j] = y.key[j + t]
	if not y.leaf:
		for j in range(0, t - 1):
			z.c[j] = y.c[j + t]
	y.n = t - 1
	for j in range(x.n, i + 1, -1):
		x.c[j + 1] = x.c[j]
	x.c[i + 1] = z
	for j in range(x.n - 1, i, -1):
		x.key[j + 1] = x.key[j]
	x.key[i] = y.key[t]
	x.n = x.n + 1
	self.disk_write(y)
	self.disk_write(z)
	self.disk_write(x)
```

## Insert-nonfull
- Add a key `k` to a node `x`
- `x` must not be full
- If `x` is a leaf, add `k` to the list of keys
	- Does a [[#Disk-write]]
- Otherwise: find the correct leaf node to insert `k` into
	- Does a [[#Disk-read]]
	- The appropriate node may be full. Call [[#Split-child]] if required
		- Recur on the newly-split child

## Insert
- Add a key into a B-tree
- If the root is full, call [[#Split-child]]
	- Involves allocating a node
- Total CPU time: $P(t \log_{t} n)$
- $O(\log_{t} n)$ disk accesses

```python
def insert(self: BTree[T], key: T) -> None:
	r = self.root
	if r.n == 2 * self.t - 1:
		s = allocate_node()
		self.root = s
		s.leaf = False
		s.n = 0
		s.c[0] = r
		self.split_child(s, 0)
		self.insert_nonfull(s, key)
	else:
		self.insert_non_full(r, key)
```