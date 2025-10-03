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

# Parallellism
- Work is distributed across multiple cores
	- The work is *divided*
- Computed *at the same time*
- Goal: process a large dataset quickly
- A program has to be written with parallelism in mind

## Data-parallelism
- Splitting data into chunks/distributing them among workers

## Task-parallelism
- Running multiple jobs in parallel
- Flexible
	- Tasks can create more tasks as required
- The result is updated incrementally
	- Nice for rendering progress bars
- 
## Metrics
- Core utilisation: are all cores close to full utilisation?

# Concurrency
- Jobs are run concurrently
	- Interactions between actors
- Uses an event loop or threads
	- Event loops interleave tasks
- Useful for I/O bound applications

![Concurrency vs parallelism](https://s3-wp-product.s3.amazonaws.com/wp-content/uploads/20240308182308/1_5P4uAgYGrsl4Lq-4ASitEQ.png)

## Metrics
- Requests per second
- Service-level agreements (SLAs)
	- Uptime
	- Response latency
	- Quality of service
- Delivery requirements: business requirements
	- E.G the system has to handle 30 billion messages per day
- Mobility: the ability to be fault-tolerant and move microservices between clouds

# Distributed systems
- Distributed systems employ multiple hosts
- Typically across a network
- Increases reliability via redundancy
	- Can fall-back to other hosts if one fails
	- However there are more points of failure
		- You're more likely to encounter a failure when you have 100 devices vs 1
- Overcomes limits to vertical scaling

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