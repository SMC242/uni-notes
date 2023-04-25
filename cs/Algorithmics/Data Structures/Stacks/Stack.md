---
tags:
  - ADT/Stack
---
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
- [[Queue]]s are FIFO (first-in-first-out)

# Members
- The internal array
- `top`:  the most recently added element

# Operations
## Push
- Add an element to the top of the stack

## Pop
- Remove and return the top element
- Popping from an empty stack is called a "stack underflow"

# Optional operations
These operations do not need to be defined

## Peek
- Get the top element without removing it
- Also known as `top`

## Size
- Get the number of elements in the stack

## Empty
- Check whether the stack is empty

# Implementations
All implementations of this [[Abstract Data Type|ADT]]
```dataview
LIST
FROM
	#ADT/Stack
WHERE
	file.name != this.file.name
```