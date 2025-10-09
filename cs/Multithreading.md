---
tags:
  - Multithreading
  - Parallelism
  - DistributedSystems
---
# Overview
- Each core in a CPU has multiple threads. These threads are executed in [[Parallellism|parallel]]
- Operating systems map OS threads to physical threads
- Threads share memory

# Critical regions
- Areas where shared resources must be consistent
	- E.G reading a shared value

# Green threads
- Threads managed by the runtime instead of the operating system
- Faster to acquire
	- Because you don't need to defer to the OS
- The runtime manages provisioning OS threads
	- The OS will then dispatch these to hardware threads
# Communication
- Communication happens over some shared resource
	- Shared memory
	- File handles
	- Network interfaces

## Mutexes
- Stands for "mutual exclusion"
- AKA locks
- One thread *acquires* a lock for a resource and others can't access the resource until the lock has been *released*
- Uses [[Compare-and-swap]] instructions under the hoop

## Condition variables
- A way to signal that something has happened
- An alternative to busy waiting (E.G polling)
- Threads `wait` for a condition variable to be set via `signal`

## Semaphores
- A [[#Mutexes|mutex]] and [[#Condition variables|condition variable]] with a counter
- Used to represent multiple available handles for a resource
- `wait` decrements the counter, blocks if `0`
- `signal` increments the counter and wakes up waiting threads

## Futures
 - A higher-level construct where you can pass around uncomputed values
 - Calling `get()` will block until the value is computed
 - Concept used in lots of languages
	 - C++
	 - C#
	 - Rust
	 - JavaScript
	 - Python

See [the C++ docs on futures](https://en.cppreference.com/w/cpp/thread/future.html)

# Types of failures
## Deadlock
- Multiple threads are unable to continue
	- E.G are both waiting for access to a resource
- Common cause: a lock wasn't released

## Livelock
- Threads are able to communicate, but don't achieve anything
	- E.G both threads are able to read a file, but no threads are putting data into it

# Multithreading constructs
Here are some abstractions for multi-threading

## Futures
- AKA promises, coroutines
- A box that will contain a value in the future

## Barriers
- Prevents threads from progressing until they have all reached the same checkpoint
	- For resumability --> fault tolerance
- Implemented as a counter
	- Under the hood, just a [[#Semaphores|semaphore]] where `wait` increments a counter

## Threadpool
- A struct that manages a reusing set of threads to avoid paying the cost of respawning OS threads for each task
	 - To spawn a thread, the program must do a system call
# See also
- [[Hyperthreading]]