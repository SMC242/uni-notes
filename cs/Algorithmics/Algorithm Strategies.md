# Incremental
- Work one element at a time

# Divide and conquer
- A [[Recursion|recursive]] approach

## Method
- Divide: split the problem into smaller sub-problems 
- Conquer: solve the sub-problems recursively until the [[Recursion#Base case|base case is reached]]
- Combine: recombine the sub-problems to create the solution for the original problem

# Dynamic programming
- A [[Recursion|recursive]] approach
- Only works with [[Functional Programming Terms#Pure function|pure functions]]
	- This is because the result of solving a function must be deterministic to be cached

## Method
- Split the problem into sub-problems
- Solve each problem and [[Caching#Memoisation example|cache]] the result
- Instead of recalculating results, use the cache