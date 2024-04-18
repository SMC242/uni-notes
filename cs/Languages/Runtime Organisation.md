# Overview
This note covers how different programming languages organise memory at runtime

See first:
- [[Types]]

# Arrays
- Arrays are stored in contiguous blocks of memory
- Elements are accessed via their offset from the start of the block
	- $o_i = Size(T) \times i$


# Cartesian products
- Stored the same way as arrays
- Offsets are calculated as the sum of the previous sizes
	- $o_{i} = \sum\limits_{i} Size(T_i)$


# Objects
- Since they're just tagged tuples, add a tag indicating its class
- Stored like tuples with an extra tag

# Heap manager
- Part of the runtime that knows where the free space in the [[Stack and Heap#Heap|heap]] is
	- Maintains a "free list": a [[Linked List]] of free spaces and their size
- Has two functions:
	- Create: create a heap variable
		- Employed by the allocator
	- Destroy: destroy a heap variable
		- Employed by the deallocator

See also:
- [[Variables And Lifetimes]]
- [[Garbage Collection]]