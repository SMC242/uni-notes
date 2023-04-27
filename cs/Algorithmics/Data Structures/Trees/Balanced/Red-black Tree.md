---
tags: ADT/SelfBalancingTree 
---

# Overview
- Which [[Abstract Data Type]] does this data structure implement?
- What are its implementation details?

A [[Self-balancing Tree]] that labels nodes as either red or black. 

# Properties
- Basic operations have $O(\log n$ at the worst case)
- Good for frequently-changing data
	- Used in the Linux task scheduler

## Red-black properties
1. All nodes have a colour in $\{red, black\}$
2. The root node must be black
3. Leaf nodes must be black
4. The children of red nodes are both black
5. When creating a simple path between a node and a descendant, all possible paths will have the same number of black nodes
	- This means that there is no bad path

## Node values
Nodes are separated by their properties:
- Leaf nodes contain `NIL`s
	- This is stored as a single sentinel `RedBlackTree.NIL` that is pointed to
	- This avoids allocating lots of unique `Node.NIL`s and simplifies the code
- Internal nodes have normal values
	- Only internal nodes contain values

This means that the type of `key` is `T | NIL`

## Black height
- $bh(x)$ is the number of nodes between $x$ and a leaf node

# Members
Each node has:
- `colour`: a value in $\{red, black\}$
![[cs/Algorithmics/Data Structures/Trees/Binary Tree#Members]]

# Operations
## Rotation
- Fix a tree so that the [[#Properties#Colours|red-black properties]] hold
	- Update colours
	- Apply [[#left rotation|left and right rotations]]
- [[Time Complexity]]: $O(1)$

### Left rotation
- The tree gets moved around the link between `x` and `y`
- Used when the tree is too heavy on the right branch 

Assumptions:
- The right child of `x` can't be `NIL`
- The root's parent must be `NIL`

![Left rotation diagram](http://www2.cs.ccu.edu.tw/~tmh104u/rotate5.png)

```python
def left_rotate(self: RedBlackTree[T], x: Node[T]) -> None:
	y = x.right
	x.right = y.left
	if y.left != RedBlackTree.NIL:
		y.left.p = x
	y.p = x.p
	if x.p == RedBlackTree.NIL:
		self.root = y
	elif x == x.p.left:
		x.p.left = y
	else:
		x.p.right = y
	y.left = x
	x.p = y
```

### Right rotation
- Used when the tree is too heavy on the left branch

![Right rotation example](http://www2.cs.ccu.edu.tw/~tmh104u/rotate3.png)

The code is similar to [[#Left rotation]]

## Insertion
- Create a new red node
- Insert it in the correct place, as in [[Binary Search Tree]]
- Find and fix violations of the [[#red black properties]] using [[#Fixup]]

## Fixup
![[#Red-black properties]]
After a node is inserted, some properties may be violated:
- If the new node is the root: property 2
- If the new node's parent is red: property 4

![Family diagram](https://ds2-iiith.vlabs.ac.in/exp/red-black-tree/red-black-tree-oprations/images/uncle.png)

### Function
```python
def fixup(self: RedBlackTree[T], node: Node[T]) -> None:
	while node.p.colour == Colours.RED:
		# Left branch
		if node.p = node.p.p.left:
			y = node.p.p.right
			# Case 1
			# `node`'s uncle `y` is red: flip colours
			if y.colour == Colours.RED:
				node.p.colour = Colours.BLACK
				y.colour = Colours.BLACK
				node.p.p.colour = Colours.RED
				node = node.p.p
			else:
				# Case 2
				# Uncle is black and `node` is a right child:
				# convert to case 3
				if node == node.p.right
					node = node.p
					self.left_rotate(node)
				# Case 3
				# Uncle is black and `node` is a left child:
				# change parent, grandparent, rotate right
				node.p.colour = Colours.BLACK
				node.p.p.colour = Colours.RED
				self.right_rotate(node.p.p)
		# Right branch (symmetrical to previous code)
		elif node.p == node.p.p.right:
			y = node.p.p.left
			# Case 1
			# `node`'s uncle `y` is red: flip colours
			if y.colour == Colours.RED:
				node.p.colour = Colours.BLACK
				y.colour = Colours.BLACK
				node.p.p.colour = Colours.RED
				node = node.p.p
			else:
				# Case 2
				# Uncle is black and `node` is a left child:
				# convert to case 3
				if node == node.p.left
					node = node.p
					self.right_rotate(node)
				# Case 3
				# Uncle is black and `node` is a right child:
				# change parent, grandparent, rotate right
				node.p.colour = Colours.BLACK
				node.p.p.colour = Colours.RED
				self.left_rotate(node.p.p)
	# The root must be black
	self.root.colour = Colours.BLACK
		
```