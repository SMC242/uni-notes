---
tags: ADT/SelfBalancingTree 
---

# Overview
- Which [[Abstract Data Type]] does this data structure implement?
- What are its implementation details?

A [[Self-balancing Tree]] that labels nodes as either red or black. 

# Properties
- Basic operations have $O(\log n$ at the worst case

## Colours
1. All nodes have a colour in $\{red, black\}$
2. The root node must be black
3. Leaf nodes must be black
4. The children of red nodes are both black
5. When creating a simple path between a node and a descendant, all possible paths will have the same number of black nodes
	- This means that there is no bad path

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