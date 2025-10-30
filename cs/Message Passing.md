---
tags:
  - DistributedSystems
aliases:
---
# Overview
- A system for [[Parallellism|parallel]] [[cs/Computational Models#Distributed systems|distributed systems]] where data is shared by sending messages between hosts
	- [[cs/Computational Models#Distributed memory|Distributed memory model]] so you don't need to manage [[Multithreading#Critical regions|critical regions]]
	- You explicitly send messages to other hosts and receivers decide where to store it in memory
- Most HPC environments are designed for this

> [!NOTE] Multi-processing
> Runs multiple processes (I.E with their own memory space). In the case of Intel [[MPI]], Hydra manages networking between processes

# See also
- [[MPI]]: a language-agnostic API for message passing
- [[BEAM VM]]: Erlang's virtual machine, implements message passing