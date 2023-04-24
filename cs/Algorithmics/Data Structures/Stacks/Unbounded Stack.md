# Unbounded stacks
- Space complexity: $O(cs)$ where $c$ is some constant and $s$ is the current size
- No stack overflow errors
- Easy to implement

## Resizing
- This type of stack resizes the internal array as required
- Double the internal array's size when the stack fills up
- Half the internal array when it's a quarter full
- Increasing the size by a constant means that adding $n$ elements takes $O(n)$
	- Insertion is constant time *on average* ("amortised constant time")
- Holds the maximum size as `n`

# Operations
## Resize
- Resize the internal array to size $n$
- [[Time Complexity]]: $O(n)$

```java
protected static void resize(int n) {
	T[] newData = new T[n];
	for (int i = 0; i < this.top; i++) {
		newData[i] = this.data[i];
	}
	this.data = newData;
}	
```

## Pop
- `pop` can still underflow
```python
def pop(self) -> T | None:
	if self.empty():  return None
	x = self.data[self.top]
	self.top -= 1
	if self.top > 0 and self.top == self.n / 4:
		self.resize(self.n / 2)
	return x
```

## Push
- This will never overflow
```python
def push(self, x: T) -> None:
	if self.top = self.n - 1:
		self.resize(n * 2)
	self.top += 1
	self.data[self.top] = x
```