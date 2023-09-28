---
tags:
- ADT/Stack

---
# Overview
A [[Stack]] that has a maximum number of elements. It uses a static array to store elements

See also: [[Stack]]

# Properties
- Space complexity $O(size)$
- All operations are $O(1)$
- Stack size cannot be changed without reallocating the internal array
- Generates errors (stack overflow)

# Members
- `data`: the internal array
- `top`: an index for the most recently added element
	- `top = -1` if the stack is empty

# Operations
## Push
[[Stack#Push]]

- Pushing to a full bounded stack is called a "stack overflow"
- [[Time Complexity]]: $O(1)$

```python
def push(self, x: T) -> None:
	if self.top + 1 == self.bound:  raise StackOverflowError()
	self.top += 1
	self.data[self.top] = x
```

## Pop
[[Stack#Pop]]

- [[Time Complexity]]: $O(1)$

```python
def pop(self) -> T | None:
	if self.empty():  return None
	self.top -= 1
	return self.data[self.top + 1]
```

# Optional operations
These operations do not need to be defined

## Peek
[[Stack#Peek]]

- [[Time Complexity]]: $O(1)$

```python
def peek(self) -> T | None:
	if self.top == -1:  return None
	return self.data[self.top]
```

## Size
[[Stack#Size]]

- [[Time Complexity]]: $O(n)$ unless a `length` counter is held

```python
def size(self) -> int:
	return self.top + 1
```

## Empty
[[Stack#Empty]]

- [[Time Complexity]]: $O(1)$

```python
def empty(self) -> bool:
	return self.top == -1
```
`
