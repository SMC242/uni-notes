# Definition
A [[Queue]] that has a maximum capacity. It uses a static array to store elements

See also: [[Bounded Stack]]

# Properties
- Space complexity is $O(n)$
- All operations are $O(1)$
- The size can't be changed without reallocating the internal array
- Vulnerable to overflows

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
[[Queue#Enqueue]]

- Queuing to a full bounded queue causes an overflow
- [[Time Complexity]]: $O(1)$

```python
def enqueue(self, x: T) -> None:
	if self.size() == n - 1:  raise QueueOverflowError()
	self.data[self.tail] = x
	self.tail = (self.tail + 1) % self.n
```

## Dequeue
[[Queue#Dequeue]]

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
[[Queue#Front]]

- [[Time Complexity]]: $O(1)$

```python
def front(self) -> T | None:
	if self.empty():  return None
	return self.data[self.head]
```

## Queue-empty
[[Queue#Queue-empty]]

- [[Time Complexity]]: $O(1)$

```python
def empty(self) -> bool:
	return self.head == self.tail
```

## Queue-size
[[Queue#Queue-size]]

- [[Time Complexity]]: $O(1)$

```python
def size(self) -> int:
	return (self.n - self.head + self.tail) % self.n
```
