---
tags:
  - C
---
# Overview
The memory used by a program is split into two sections: the [[#Stack]] and the [[#Heap]]. This note will focus on C, but is still worth knowing, even for languages that automatically manage memory

# Stack
- Memory for things with a static size known at compile time
	- Variables that are initialised (`char c = 'a';` or `int xs[4];`)
	- [[Constants]]
- Automatically allocated
- These are stored in the function's stack frame

# Call stack
- Each function's variables are saved in a stack frame in the [[#call stack]]
	- Stack frames are freed once the function has finished executing
- This allows [[Recursion]] and [[Closures]]

## Stack overflows
- Since the stack and heap share the same address space (I.E amount of memory allocated to the program), increasing the size of one reduces the space for the other
- Running out of stack memory is called a "stack overflow" error
- To put a limit on runaway [[recursion]], most languages impose a recursion limit
	- Python's limit is 1000
	- Surpassing that limit is called a "recursion depth"

# Heap
- Memory that can be dynamically requested
- Must be manually managed

## Malloc
`malloc` is used to request a block of memory
```c
type * eightBytes = malloc(8);
```

> [!WARNING]
> If memory allocation fails, `NULL` will be returned. Always check for this case!

## Free
To release the block of memory, use `free`

```c
free(ptr);
ptr = NULL;
```

> [!DANGER]
> You must release a block of memory after allocating it. This will not happen automatically