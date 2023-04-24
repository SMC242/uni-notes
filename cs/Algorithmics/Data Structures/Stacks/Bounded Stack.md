# Definition
A stack is like a pile; they follow the last-in-first-out policy (LIFO)

# Motivation
## Use case
- Nested structures
	- Call stacks
	- Page history in web browsers
	- "Undo" features in text editors
	- Syntax parsing

# Comparison
## Stack vs Queue
- Stacks are LIFO (last-in-first-out)
- [[Bounded Queue]]s are FIFO (first-in-first-out)

# Members
- The internal array
- `top`: an index for the most recently added element
	- `top = -1` if the stack is empty

# Operations
## Push
- Add an element to the top of the stack
- Pushing to a full bounded stack is called a "stack overflow"
- [[Time Complexity]]: $O(1)$

```python
def push(self, x: T) -> None:
	if self.top + 1 == self.bound:  raise StackOverflowError()
	self.top += 1
	self.data[self.top] = x
```

## Pop
- Remove and return the top element
- Popping from an empty bounded stack is called a "stack underflow"
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
- Get the top element without removing it
- Also known as `top`
- [[Time Complexity]]: $O(1)$

```python
def peek(self) -> T | None:
	if self.top == -1:  return None
	return self.data[self.top]
```

## Size
- Get the number of elements in the stack
- [[Time Complexity]]: $O(n)$ unless a `length` counter is held

```python
def size(self) -> int:
	return self.top + 1
```

## Empty
- Check whether the stack is empty
- [[Time Complexity]]: $O(1)$

```python
def empty(self) -> bool:
	return self.top == -1
```

# Properties of bounded stacks
- Space complexity $O(size)$
- All operations are $O(1)$
- Stack size cannot be changed without reallocating the internal array
- Generates errors (stack overflow)