# Overview
- Time complexity is a mathematical function that describes how the amount of time it takes to run the function *grows*
- Do not confuse this with [[Running Time]]

# Cases
<dl>
	<dt>Best case</dt>
	<dd>The function is called with an input that makes it exit early</dd>
	<dt>Worst case</dt>
	<dd>The situation where the function runs through the entire input (E.G element not found when searching)</dd>
</dl>
# Big-Oh Notation
![[Asymptotic Notation#Big-Oh Notation]]
# Growth rate
- The growth rate of a function is a property of the algorithm - it's always the same
- Having certain guarantees about the input can be used to write more efficient algorithms
	- a `maximum(xs)` function would take constant time if `xs` was always sorted in descending order (the biggest element is first)
	- This is how many algorithms work - they use an an appropriate data structure for the problem

# Example growth rates
- $O(1)$ constant time - some data structures offer functions like this
- $O(log n)$ - binary search
- $O(n)$ linear time - the holy grail of time complexities
- $O(n \log_2 n)$ logarithmic time - most sorting algorithms
- $O(n^2)$ quadratic time - getting into the danger zone
- $O(n^x)$ polynomial time - some matrix operations are $O(n^3)$
- $O(2^n)$ exponential time - not feasible
- $O(n!)$ factorial time - not feasible

![Time complexities graphed](https://miro.medium.com/max/1200/1*5ZLci3SuR0zM_QlZOADv8Q.jpeg)

# Simplifying growth rates
When talking about the growth rate of a function, ignore:
- Constant factors 
	- $+ 2$
	- $- 47^3$
- Lower-degree terms
	- $2n^{2} + 4n$ (quadratic function, ignore the linear term)
	- $3n^{3} + 2n^{2} + n$ (cubic function, ignore the other terms)

# Analysing recursive functions
The time complexity of a recursive function is $O(a^{h + 1})$ where $a$ is the number of recursive calls each node can make (E.G. 1 for linear, 2 for binary) and $h$ is the height of the tree

![[Recursion#Analysing trees]]