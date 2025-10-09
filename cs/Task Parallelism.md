---
tags:
  - DistributedSystems
  - AsynchronousProgramming
  - Parallelism
---
# Overview
- Abstract functions are executed by threads
	- Threads are not spawned to do a specific function
	- The spawning thread can "wait" on the result
- Tasks can spawn other tasks, making this model flexible
- Often known as "async" in systems programming languages (but high-level languages like JavaScript and Python use [[Event Loops]])