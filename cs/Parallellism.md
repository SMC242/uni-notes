---
tags:
  - DistributedSystems
  - Parallelism
---
# Overview
- Work is distributed across multiple cores
	- The work is *divided*
- Computed *at the same time*
- Goal: process a large dataset quickly
- A program has to be written with parallelism in mind

# Fork-join parallelism
![[Fork-join Parallelism]]

## Data-parallelism
- Splitting data into chunks/distributing them among workers

## Task-parallelism
![[Task Parallelism]]

## Metrics
- Core utilisation: are all cores close to full utilisation?