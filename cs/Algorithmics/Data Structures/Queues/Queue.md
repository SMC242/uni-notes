---
tags:
  - ADT/Queue
---

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
![[Stack#Comparison]]

# Members
- `data`: where the elements are stored
- `head`: the first element
- `tail`: an index for the next free slot in the queue
- `n`: the maximum size of the queue

# Operations

## Enqueue
- Add an element to the end of the queue#

## Dequeue
- Remove and return the top element of the queue

# Optional operations
These operations do not have to be defined

## Front
- Get the first element without removing it

## Queue-empty
- Get whether the queue is empty

## Queue-size
- Get the number of elements in the queue

# Implementations
All of implementations of this [[Abstract Data Type|ADT]]:

```dataview
LIST
FROM
	#ADT/Queue
WHERE
	file.name != this.file.name
```