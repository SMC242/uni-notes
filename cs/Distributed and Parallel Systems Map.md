---
tags:
  - DistributedSystems
---

# Overview
Some computations or systems are infeasible to compute on a single CPU core and must be spread across multiple

# Index
```dataview
LIST
FROM
	#DistributedSystems 
WHERE
	file.name != this.file.name
```

# Topics

- [[cs/Computational Models|Computational Models]]: an overview of the different ways to distribute computation

## Parallelism
![[Parallellism#Overview]]

- [[Parallellism]]
- [[Multithreading]]: how to utilise multiple threads and communicate between them
- [[POSIX threads]]: a low-level API for [[Multithreading]]
- [[OpenMP]]: a runtime for high-level parallelism

## Concurrency
![[Concurrency#Overview]]

- [[Concurrency In Go]]: using Goroutines to achieve concurrency
# See also
- [[Systems Programming Map]]: a lot of parallelism is done with systems programming languages so the topics overlap heavily