# Definition
A queue is an ordered collection that follows the first-in-first-out policy (FIFO)

# Motivation
## Use case
- Doing operations sequentially
	- Long-running functions
	- Requests
- Waiting lists
- Accessing shared resources
	- Printers
	- Servers
- Multitasking
	- [[Asynchronous Programming]]
	- [[Multithreading]]

# Comparison
![[Bounded Stack#Comparison]]

# Members
- `data`: the internal array (if the queue is bounded)
- `head`: an index for the first element
- `tail`: an index for the next free slot in the queue
	- This is why a bounded queue stores $n - 1$ elements
- `n`: the maximum size of the queue

Bounded queues use circular indexes:
- When `tail = n - 1`, set `tail = 0`
- When `head = n - 1`, set `head = 0`

# Operations
Note that modulo (`%`) is used to wrap the indexes

## Enqueue
- Add an element to the end of the queue
- Queuing to a full bounded queue causes an overflow
- [[Time Complexity]]: $O(1)$

```python
def enqueue(self, x: T) -> None:
	if self.size() == n - 1:  raise QueueOverflowError()
	self.data[self.tail] = x
	self.tail = (self.tail + 1) % self.n
```

## Dequeue
- Remove and return the top element of the queue
- Calling this on an empty queue causes an underflow
- [[Time Complexity]]: $O(1)$

```python
def dequeue(self) -> T | None:
	if self.empty():  return None
	x = self.data[self.head]
	self.head = (self.head + 1) % self.n
	return x
```

# Optional operations
These operations do not have to be defined

## Front
- Get the first element without removing it
- [[Time Complexity]]: $O(1)$

```python
def front(self) -> T | None:
	if self.empty():  return None
	return self.data[self.head]
```

## Queue-empty
- Get whether the queue is empty
- [[Time Complexity]]: $O(1)$

```python
def empty(self) -> bool:
	return self.head == self.tail
```

## Queue-size
- Get the number of elements in the queue
- [[Time Complexity]]: $O(1)$

```python
def size(self) -> int:
	return (self.n - self.head + self.tail) % self.n
```

# Properties of bounded queues
- Space complexity is $O(n)$
- All operations are $O(1)$
- The size can't be changed without reallocating the internal array
- Vulnerable to overflows