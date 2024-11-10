# Overview
There are a variety of general solvers. These solvers are generally good for their problem type, but can run in exponential time in the worst case

- Search-based solver
- SAT solver: specialised for finding solutions for a given boolean formula
- IP solver: specialised for integer programming
- SIMPLEX
- Not-guaranteed-to-be-optimal heuristic solver: won't necessarily give the best answer, but will give a good one quickly

# Generate-and-test
- Generate a solution, check if it meets all the constraints
- Naive approach because it's not guided by the shape of the constraints at all

# Backtrack search
- Worst case: exponential time
	- Generally better
- Uses [[Depth First Search]] to search the solution space, going down branches until one constraint is unsatisfied and pruning the branch from the tree

# Arc consistency
![[Arc consistency]]

# Integer [Linear] Programming
![[Integer Programming]]