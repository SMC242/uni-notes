# Overview
- Each core in a CPU has multiple threads. These threads are executed in parallel
- Operating systems map OS threads to physical threads
- Threads share memory

# Lifecycle
1. Fork: branch off a new thread
2. Execute...
3. Join: wait for a thread to finish

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

## Condition variables
- A way to signal that something has happened
- An alternative to busy waiting (E.G polling)
- Threads wait for a condition variable to be set

## Semaphores
- A [[#Mutexes|mutex]] and [[#Condition variables|condition variable]] with a counter
- Used to represent multiple available handles for a resource
- `wait` decrements the counter, blocks if `0`
- `signal` increments the counter and wakes up waiting threads

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
- Implemented as a counter
	- `wait` increments the counter