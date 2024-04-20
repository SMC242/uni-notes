---
aliases:
  - GC
---

# Overview
A method of automatic memory management where the runtime periodically deallocates unused [[Stack and Heap#Heap|heap]] variables

- Less cognitive load on the programmer --> less bugs
- Reduces runtime performance

# Algorithms
## Mark-and-sweep
- Traverses the heap variables and their [[pointers]], marking reachable variables
	- Uses [[Depth First Search]]
- Unmarked variables are therefore unreachable and will be deallocated
- [[Time Complexity]]: $O(n_{r} + n_h)$
	- $n_r$: the number of reachable variables
	- $n_h$: the total number of heap variables
- Causes fragmentation
	- Solution: combine adjacent free areas after sweeping

## Copying
- Uses two spaces in the heap
- Initialisation: space 1 contains all heap variables. Space 2 has nothing
	- This groups used memory into one place
	- Cost: you have to update lots of pointers

Process: 
- When the GC finds a marked variable, it moves it to space 2
- After the GC runs, space 1 and 2 are swapped

## Generational
- Uses two or more spaces
	- Old: long-lived variables
	- Young: short-lived variables
- Garbage collection runs less frequently on the old space
- Variables that survive a certain number of generations get *promoted*to the old space

## Reference counting
- A count of references is maintained for each variable
- When the count reaches 0, it is deallocated