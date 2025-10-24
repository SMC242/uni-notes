---
tags:
  - DistributedSystems
---
# Overview
There are multiple ways to compute more stuff

# Memory models
- Locality is better than 
## Shared memory
- A single address space is shared by several [[CPU]] cores
- Examples: [[POSIX threads]], [[Concurrency In Go|Goroutines]], [[OpenMP]]
	- Threads must use [[Multithreading#Communication|synchronisation primitives]]
- High bandwidth, low latency
- Locality is generally not a problem unless you're optimising down to the assembly level
- Can't scale past 32 cores per socket/memory bus before contention kicks in

## Distributed memory
- Synchronisation via messages
- Low bandwidth, high latency **but** you can scale to hundreds of cores
	- Used for cloud, computing clusters, and high-performance computing

## Hybrid memory
- Most real-world systems are hybrid
- Lots of shared-memory programs running, connected via [[Networking Map|networks]]
- Different types of memory may be used on the same machine (CPU and GPU)

> [!NOTE] Non-uniform Memory Access
> Some memory blocks are closer to certain [[CPU]] cores, making them faster. We usually ignore this fact because it's inconvenient to program around

## Optimising hybrid setups
Consider the following:
- Where should computations happen? Ideally as close to the computer that needs the answer as possible
- Where should the data be stored? Ideally as close to the above as possible
	- Should it be [[Data Replication|replicated]]?
- How will data be communicated? [[HTTP]], [[Protobufs]]?
	- Sometimes it's faster to recompute than communicate data

# Parallelism
![[Parallellism]]

# Concurrency
![[Concurrency]]

# Distributed systems
- Distributed systems employ multiple hosts
- Typically across a network
- Increases reliability via redundancy
	- Can fall-back to other hosts if one fails
	- However there are more points of failure
		- You're more likely to encounter a failure when you have 100 devices vs 1
- Overcomes limits to vertical scaling


## Metrics
- Dynamic scalability: how well can you scale the system to incoming traffic?
- Fault tolerance
- Self-healing: can it automatically recover from being overloaded and failing? Nobody wants to get paged at 03:00

# Design considerations
- Partitioning: what should be broken into chunks
	- And how large the chunks should be
- Placement: local vs distributed computation
- Communication: what should be sent between hosts
	- Sometimes it's more efficient to recompute
- Synchronisation
- Scheduling: there may be an order that threads need to run in
- Fault tolerance
	- Detection
	- Recovery