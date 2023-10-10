# Overview
- A [[Recursion|recursive]] approach to solving problems
	- Often used in solutions that re-use previous results, such as optimisation problems like *Buy as many items as possible for the least price*
- Considers a problem as a series of sub-problems
	- The solution is built by solving the sub-problems and storing them in a table ([[Caching#Memoisation example|memoising]] them)
- Only works with [[Functional Programming Terms#Pure function|pure functions]]
	- This is because the result of solving a function must be deterministic to be cached