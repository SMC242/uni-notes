---
tags:
  - Multithreading
  - Parallelism
  - DistributedSystems
---
# Overview
OpenMP is a compiler extension for a variety of compiled languages, including C and Fortran. It used high-level compiler directives to add parallelism to a program.

- [[cs/Computational Models#Shared memory|Shared memory]] model
- [[cs/Computational Models#Parallellism|Parallel]] model
- Can do [[Fork-join Parallelism]] and [[Task Parallelism]]
- Portable between operating systems
- Can add parallelism to a program incrementally
	- It's better to use [[POSIX threads|pthreads]] if you need fine-grained control
- Very complex API
- Limited error handling

> [!NOTE] Syntax
> Directives apply to blocks
> 
> ```c
> include <omp.h>
> 
> #pragma omp parallel
> {
> 	// Do stuff
> }
> ```
> 
> 
> Each block ends with a [[Multithreading#Barriers|barrier]]

# Compiler flags
## Linking the library
### GCC
```bash
gcc -fopenmp
```

### Clang
```bash
clang -fopenmp=libiomp5
```


## Limiting thread count
- Set the `OMP_NUM_THREADS` environment variable

# Directives
## Parallel

- Spawn a bunch of threads to process the block
- All other directives must be contained within one of these blocks or they won't have access to a [[Multithreading#Theadpool|threadpool]]
```c
#pragma omp parallel
{  // Fork here
	// Do stuff
}  // Join here
```

## For
```c
#pragma omp for
for (int i = 0; i < 1_000_000; i++) {
	// Do stuff
}
```

## Ownership

- Can be added to any directive
- Marks variables as shared/not shared between the threads
- Default visibilities:
	- Inside a block: private
	- Outside a block: shared

Shared:
```c
#pragma omp parallel for shared(x, y)
```

> [!WARNING] Synchronisation
> Shared variables are not automatically [[Multithreading#Communication|synchronised]]. You still need to add [[Multithreading#Mutexes|mutexes]]

Private (the default if not specified):
```c
#pragma omp parallel for private(x, y)
```

> [!WARNING] Initialisation
> You must initialise a private variable within the block because it will be uninitialised by default

## Schedule

- Allows you to set chunking
- Static has faster initialisation because it knows ahead of time which jobs to send where
	- Use when jobs have a constant computation time
- Dynamic is slower to initialise but performs better when jobs have a variable computation time

Static:
```c
// Split into chunks of 4 and divide in round-robin
#pragma omp for schedule(static, 4)
```

Dynamic (work-stealing):
```c
// Work-stealing: divides the work into jobs of size 4.
// Idle threads can take jobs
#pragma omp for schedule(dynamic, 4)
```

Auto:
```c
// The compiler will decide which strategy to use
#pragma omp for schedule(auto)
```

## Reduction

- Fork a bunch of threads and reduce the work into one value

```c

// Implicitly shared
float result = 0.0f;
#pragma omp parallel for reduction(+:result)
for (int i = 0; i < 100; i++){
	result = result + (xs[i] * ys[i]):
}
```

## Nowait

- Disables the implicit barrier at the end of a block

```c
#pragma omp parallel for nowait
for (int i = 0; i < 100; i++){
	// Do stuff
}

// Threads that finish will move onto this block
#pragma omp parallel for
for (int i = 0; i < 100; i++){
	// Do more stuff
}
```
## Critical

- [[Multithreading#Communication|Synchronises]] a block

```c
int continue = 1;

#pragma omp parallel for shared(continue)
for (int i = 0; i < 100; i++) {
	#pragma omp critical
	{
		if (continue == 0) break;
	}
	// Do stuff
	if (condition) {
		#pragma omp critical
		{
			continue = 0;
		}
	}
}
```

## Sections

- Allows you to say "one thread will do this block"

```c
#pragma omp parallel sections
{
	#pragma omp section
	{
		// Do stuff with thread 1
	}
	
	#pragma omp section
	{
		// Do stuff with thread 2
	}
}
```

# Task-parallelism directives

OpenMP supports [[Task Parallelism]] too

## Single

- Only one thread from the [[Multithreading#Threadpool|threadpool]] will execute the block
- Typically used in [[Task Parallelism]] to spawn a function that will spawn tasks such as spawning a [[QuickSort]] that will spawn tasks for each partition

```c
#pragma omp single
{
	// Do stuff
}
```

See also: https://stackoverflow.com/a/49147204

## Spawning tasks

```c
#pragma omp task
{
	// Do stuff
}  // Implicitly wait for the task to finish
```

- Tasks are tied to a single thread (E.G a thread is dedicated to executing `extractFeatures` tasks) by default
	- They can be untied with the `untied` attribute
	- This feature doesn't work on some implementations though

## Waiting

- Wait for all tasks in the scope to finish (similar to a [[Multithreading#Barriers|barrier]])

```c
#pragma omp parallel
#pragma omp single
{
	#pragma omp task
	{
		// Task 1
	}
	
	#pragma omp task
	{
		// Task 2
	}
	
	// Wait for tasks 1 and 2 to complete
	#pragma omp taskwait
}
```