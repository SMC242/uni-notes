# Overview
A [[B-Tree]] where internal nodes don't hold data pointers to reduce storage overhead. Used in [[Multilevel Index]]ing

> [!WARNING] Silver bullets
> While B+ trees are more storage-efficient than [[B-Tree]]s, they still have more storage overhead than a [[Indexing|single layer of indexes]]

## Motivation
- [[B-Tree]]s store a lot of metadata
	- Data-pointers
	- Tree-pointers
	- Search key values
- It would be ideal to increase the fan-out to increase how much the search space decreases per traversal
- Maximising the number of tree-pointers per node maximises the fan-out
- Increasing the fan-out reduces the number of layers required

See [[Multilevel Index#Redundancy]] for more information on the problem

# Structure
- $p$ is the order of the [[maths/Trees/Tree|tree]] (number of child nodes per node)

![B+ tree diagram](https://i.stack.imgur.com/qEXGR.png)

## Internal nodes
- Don't have data-pointers
- Internal nodes have pointers and keys still
	- Keys are still sorted
	- $p - 1$ keys
	- $p$ pointers
- Medians of key values are replicated in the internal nodes to make searching faster

## Leaf nodes
- Have data-pointers and keys
	- $p - 1$ data pointers
	- $p - 1$ keys
	- Keys and their pointers are sorted
- All keys in the file appear in the leaf nodes
- All leaf nodes are on the same level, so the tree is [[Balanced Trees|balanced]]
	- Constant I/O cost for traversing to a node
- Leaf nodes hold a pointer to the next leaf
	- Forms a [[Linked List]]

# Maximising order
These are the formulae for maximising order:

## Internal node

$$
p \cdot P + K \cdot (p - 1) \le B\\
$$
or
$$p \le \frac{B + K}{P+K}$$
where:
- $P$ is the size of a pointer
- $K$ is the size of a key
- $B$ is the size of a block

## Leaf node
$$p_{L} \cdot (Q + K) + P \le B$$
or
$$
p_{L} \le \frac{B - P}{Q+K}
$$
where:
- $p_{L}$ is the size of the data pointers
- $p_{L}$ is the size of the key values
- $Q$ is the size of a next-node pointer

>[!NOTE] Comparison to B-tree node
>The size of a B-tree node is:
>- $p - 1$ data pointers
>- $p - 1$ key values
>- $p$ tree pointers
>
>The formulae for maximising fan-out are:
>$$p \cdot P + (p - 1) \cdot (K + Q) \le B$$
>or
>$$p \le \frac{B + K}{P+K+Q}$$

### Leaf node size
- $p_L$ data pointers
- $p_L$ key values
- One next-node pointer $Q$

where $L$ is the number of leaves