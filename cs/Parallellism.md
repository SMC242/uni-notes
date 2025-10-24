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
	- Warning sign: high utilisation with low speedup might mean [[Multithreading#Deadlock|deadlocking/livelocking]]

## Runtime
- Ideally, there would be a logarithmic decay when adding more cores
![logarithmic decay plot](https://www.intmath.com/exponential-logarithmic-functions/svg/svgphp-graphs-exp-log-fns-2-s1.svg)

- 

# Scaling
- How do we divide work among threads efficiently?
- How do we measure speedup?

## Strong scaling
AKA speedup
$$speedup=\frac{t_{1}}{t_{n}}$$
for a fixed workload and $n$ processors

- Ideally, this would be linear
- Absolute speedup : $t_{1}$ is the runtime of a fully sequential implementation
- Relative speedup: $t_{1}$ is the runtime of a parallel implementation running on one core
	- Has the overhead of [[Multithreading#Communication|coordination]]

## Weak scaling
AKA Gustafen's law

- Concept: use parallelism to solve larger problems rather than accelerating many small problems
- Weak scaling is scaling the problem size to the number of cores available (ideally a flat line)
	- $let P = problem\ size$, for $1 core = 1P, 2 cores = 2P, ..., 8 cores = 8P$
- Used in HPC

> [!EXAMPLE] Problem size
> For a satellite imaging app: use 10km resolution for 1 core, 1km resolution for 10 cores

## Measuring efficiency
- How close the speedup is to the ideal speedup

$$eff = \frac{speedup}{p}$$ where $p$ is the linear function

### Amdhal's law
> The speedup is limited by the amount of the program that *can* be parallelised

- Scaling is rarely linear
	- There is overhead from communication
- The sequential portions of the program limit thej speedup
	- There will always be sequential parts

$$speedup = \frac{1}{\frac{(1-p)+p}{n}}$$
where $p$ is the number of parallel parts of the program

![A better explanation of the formula](https://miro.medium.com/v2/resize:fit:640/format:webp/0*_LWILJ5nXCYrKIHU.png)


![A plot of Amdhal's law](https://upload.wikimedia.org/wikipedia/commons/thumb/e/ea/AmdahlsLaw.svg/1920px-AmdahlsLaw.svg.png)