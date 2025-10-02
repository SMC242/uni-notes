# Overview
There are multiple ways to compute more stuff

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