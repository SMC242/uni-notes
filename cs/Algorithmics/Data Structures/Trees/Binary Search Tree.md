---
tags: ADT/Tree

---
# Overview
A [[cs/Algorithmics/Data Structures/Trees/Binary Tree|binary tree]] where $l <= x <= r$ where $l, x, r$ are the left, current, and right nodes' values.

# Properties
- [[maths/Trees/Binary Tree#Traversing#Inorder|Inorder traversal]] produces the values in ascending order

# Members
![[cs/Algorithmics/Data Structures/Trees/Binary Tree#Members]]

# Operations
> [!INFO]
> Operations are $O(h)$ because each step discards a subtree

## Search
- Search for a node with `key = x`
- [[Time Complexity]]: $O(h)$

```python
def search(self: Node[T], k: T) -> Node[T] | None:
	if self == Node.NIL:  return None
	elif k == x.key:   return x
	elif k < x.key:    return aux(x.left)
	else:              return aux(x.right) 
```

### Iterative version
The iterative version is typically faster

```python
def search(self: Node[T], k: T) -> Node[T] | None:
	x = self
	while x != Node.NIL and k != x.key:
		x = x.left if k < x.key else x.right
	return x if x != Node.NIL else None
```

## Minimum
- Get the smallest value in the tree
- Works by traversing to the leftmost branch
- [[Time Complexity]]: $O(h)$

```python
def minimum(self: Node[T]) -> Node[T] | None:
	# Empty tree
	if self == Node.NIL:  return None
	x = self
	while x.left != Node.NIL:
		x = x.left
	return x
```

## Maximum
- Get the largest value in the tree
- Traverses to the rightmost node in the tree
- [[Time Complexity]]: $O(h)$

```python
def maximum(self: Node[T]) -> Node[T] | None:
	# Empty tree
	if self == Node.NIL:  return None
	x = self
	while x.right != Node.NIL:
		x = x.right
	return x
```

## Successor
- Find the smallest node that is larger than the input node
- No comparisons are performed, only pointer traversal
- Returns `NIL` if the input node has the largest value in the tree
- [[Time Complexity]]: $O(h)$

```python
def successor(self: Node[T]) -> Node[T] | None:
	if self.right != Node.NIL:
		return self.right.minimum()
	# Traverse up until there is a right branch
	x, y = self, self.p
	while y != Node.NIL and x == y.right:
		x = y
		y = y.p
	return y if y != Node.NIL else None
```

## Predecessor
- Find the largest node that is smaller than the input node
- The opposite of [[#Successor]]
- $O(h)$

```python
def predecessor(self: Node[T]) -> Node[T] | None:
	if self.left != Node.NIL:
		return self.left.maximum()
	x, y = self, self.p
	while y != Node.NIL and x == y.left:
		x = y
		y = y.p
	return y if y != Node.NIL else None
```

## Size
- The number of nodes in the tree
- [[Recursion#Binary|Binary-recursive]]
- [[Time Complexity]]: $O(h)$

```python
def size(self: Node[T]) -> int:
	if self == Node.NIL:  return 0
	return self.left.size() + self.right.size() + 1
```

## Height
- The [[Rooted Tree#Height|height]] in the tree
- [[Recursion#Binary|Binary-recursive]]
- [[Time Complexity]]: $O(h)$

```python
def height(self: Node[T]) -> int:
	if self. == Node.NIL:  return 0
	# Leaf node
	elif self.left == Node.NIL and self.right == Node.NIL:
		return 0
	return max(self.left.height(), self.right.height()) + 1
```

## Insertion
- Add a new node into the tree
- This function must find the correct position for the node
	- It starts from the root and goes down
- [[Time Complexity]]: $O(h)$

```python
def insert(self: BinarySearchTree[T], node: Node[T]) -> None:
	y = Node.NIL
	x = self.root
	# Find the correct parent of `node`
	while x != Node.NIL:
		y = x
		x = x.left if node.key < x.key else x.right
	node.p = y
	# Check if root node
	if y == Node.NIL:       self.root = node
	elif node.key < y.key:  y.left    = node
	else:                   y.right   = node
```

## Transplant
- Replace a sub-tree with another
- $O(1)$

```python
def transplant(self: BinarySearchTree[T], old: Node[T], new: Node[T]) -> None:
	# Figure out where `old` relative to its parent
	if old.p == Node.NIL:
		self.root = new
	elif old == old.p.left:
		old.p.left = new
	else:
		old.p.right = new
		# Update parent
	if new != Node.NIL:
		new.p = old.p
```

## Deletion
- Remove a node from a tree
- $O(h)$

```python
def delete(self: BinarySearchTree[T], node: Node[T]) -> None:
	if node.left == Node.NIL:     # Has no left child
		self.transplant(node, node.right)
	elif node.right == Node.NIL:  # No right child
		self.transplant(node, node.left)
	else:                         # Two children
		y = node.right.minimum()  # Get successor
		if y.p != node:           # y is not directly connected
			self.transplant(y, y.right)
			y.right = node.right
			y.right.p = y
		self.transplant(node, y)
		y.left = node.left
		y.left.p = y
```

# Problems
- The height varies as nodes come and go
	- This means that the $O(h)$ running time of all of the operations will be bad if the height is large
	- Solution: [[Self-balancing Tree]]s
- Duplicates will always be put in the right branch

# Handling duplicates
Here are some strategies:
- Each node can hold a list of duplicates
- Add a `count` member
- Randomly choose whether to store the duplicate on the left or right branch
- Discard the duplicate