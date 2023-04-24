# Definition
A linked list is a list where each element holds a pointer to the next (and sometimes, the last) element. They are dynamically-sized, meaning that they can grow and shrink as required. Lends itself very well to [[Recursion|recursive algorithms]]

Note: when talking about linked lists, people are usually talking about singly linked lists unless they state that it is doubly linked

# Motivation
## Use case
- You don't know the size of the list before compile-time
- Simpler to use; Use them when memory consumption isn't a major concern
	- This avoids unnecessary memory bugs

## Comparison
### Linked list
<ul class="breakdown">
	<li class="pro">Dynamically-sized</li>
	<li class="pro">Memory usage matches the actual size of the list</li>
	<li class="con">Uses extra memory per element for the pointer(s)</li>
	<li class="con">Indexing is more expensive (bad random access)</li>
</ul>

### Arrays
<ul class="breakdown">
	<li class="pro">Fast indexing</li>
	<li class="pro">Better cache locality (sequential elements are more likely to be in the CPU's cache)</li>
	<li class="pro">Less overhead (no need to allocate memory on append)</li>
	<li class="con">If you don't fill the array, you're wasting memory</li>
	<li class="con">Requires the programmer to plan more. This may not be possible for their use case<li>
	<li class="con">Adding new elements means reallocating the whole array<li>
</ul>

# Members
## Nodes
A linked list is a chain of `Node`s. A `Node`  has the following attributes:
- `key`: the data contained within the node
- `next`: the pointer to the next node in memory
	- The last element of the list will have `next = NIL`

## Head
- A pointer (`head`) to the first element is maintained in the list instance
- `head = NIL` means the list is empty

## Tail
- Some implementations will store a pointer (`tail`) to the last node
- This fixes the problem of [[#Insert#End|append]] being $O(n)$
- Creates a new consideration: [[#insert]] and [[#delete]] have to update the tail pointer if required

# Operations
## Insert
- Adding a node to the start of the list
- [[Time Complexity]]: $O(1)$

```python
def insert(self, x: Node[T]) -> None:
	x.next = self.head
	self.head = x
```

### Insert-tail
- [[Time Complexity]]: $O(n)$
- You have to traverse to the last element

```python
def append(self, x: Node[T]) -> None:
	if self.head == Node.NIL:
		return self.insert(self, x)
	y = self.head
	while y.next != Node.NIL:
		y = y.next
	x.next = Node.NIL
	y.next = x
```

#### With tail pointer
- If the list holds a [[#Tail|tail pointer]], appending can be $O(1)$

```python
def append(self, x: Node[T]) -> None:
	x.next = Node.NIL
	if self.tail == Node.NIL:
		self.head = x
	else:
		self.tail.next = x
	self.tail = x
```

## Delete
### Delete-head
- Remove the first element
- [[Time Complexity]]: $O(1)$

```python
def delete_head(self) -> None:
	if self.head == Node.NIL:  return
	self.head = self.head.next
	# The garbage collector will clean up the removed Node
```

### Delete x
- Find and delete a node
- Worst case [[Time Complexity]]: $O(n)$

```python
def remove(self, x: T) -> None:
	if self.head == Node.Nil:  return
	y = self.head.next
	while y.next != Node.Nil:
		if y.next.value == x:
			y.next = y.next.next  # Skip node containing `x`
			return
		y = y.next
	
```

## Search
- Get the first occurrence of `key` by linear-searching
- Uses the [[Iterator]] pattern
- [[Time Complexity]]: $O(n)$

```python
def search(self, key: T) -> Node[T] | Node.NIL:
	x = self.head
	while x != Node.NIL and x.key != key:
		x = x.next
	return x  # If the end of the list was reached, `x` = NIL
```

### Recursive version
```haskell
lookup' :: Eq a => a -> [a] -> Maybe a
lookup' k [] = Nothing
lookup' k (x:xs)
    | k == x = Just x
    | otherwise = lookup' k xs
```


# Doubly-linked lists
The `Node`s used by this type of linked list also hold a pointer to the last element (`prev`). Most standard library list implementations are doubly-linked, with the notable exception of functional languages in the [ML family](https://en.wikipedia.org/wiki/Category:ML_programming_language_family) which uses singly-linked lists

<ul class="breakdown">
	<li class="pro">Allows traversal forwards and backwards</li>
	<li class="pro">Indexing becomes faster - you can start from the closest end of the list</li>
	<li class="con">Each node consumes more memory</li>
</ul>

## Delete x
- Now $O(1)$ instead of $O(n)$

```python
def remove(self, x: Node[T]) -> None:
	if x.prev != Node.Nil:
		x.prev.next = x.next  # Skip past `x`
	else:
		self.head = x.next
	if x.next != Node.Nil:
		x.next.prev = x.prev  # Skip behind `x`
```

See [[Linked List#Operations#Delete x]]

## Circular version
- Problem: there are checks for `NIL` everywhere. This is extra complexity
- Solution: a dummy node ("sentinel") that with `next = head` and `prev = tail`
- Usually a static instance attached to the class E.G `LinkedList.NilNode`

# Sorting
- [[Merge Sort]] is ideal for linked lists because it doesn't rely on random access

![[Merge Sort#Variants#Linked List]]