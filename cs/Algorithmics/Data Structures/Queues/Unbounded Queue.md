# Unbounded queues
- Resizes the internal array as needed

See [[Bounded Stack#Unbounded stacks]]

## Resize
- Resize the internal array to size $n$
- [[Time Complexity]]: $O(n)$

```java
protected void resize(int n) {
	T[] newData = new T[n];
	for (int i = 0; i < this.n - 2; i++) {
		int wrappedIndex = (this.head + i) % this.n;
		newData[i] = this.data[wrappedIndex];
	}
	this.data = newData;
	this.head = 0;
	this.tail = this.n - 1;
}
```

## Enqueue
- Can't overflow
```python
def enqueue(self, x: T) -> None:
	if self.size() == self.n - 1:  self.resize(self.n * 2)
	self.data[self.tail] = x
	self.tail = (self.tail + 1) % self.n
```

## Dequeue
- Can still underflow

```python
def dequeue(self) -> T  None:
	if self.empty():  return None
	x = self.data[self.head]
	self.head = (self.head + 1) % self.n
	if self.size() == self.n / 4:  self.resize(self.n / 2)
	return x
```