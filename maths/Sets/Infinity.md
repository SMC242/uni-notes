# Overview
Some infinite sets are countable, others are not. This doesn't mean that we *have* counted every element. Instead, it means that we could enumerate every element if we had infinite time. Example: $\mathbb{N}$

An uncountable infinity  lacks clear jumps between element. The gap is infinitely large and therefore you'd be counting between elements infinitely. An example is $\mathbb{R}$ because you'd get stuck between 0 and 1 because you can do 0.1 and 0.001 and 0.0001...

# Counting
- For a set to be countable, it must have a [[Functions#Bijective functions|bijection]] between it and another set
- To prove that a set is countable, we compare its [[Set Operations#Cardinality|cardinality]] to a subset of $\mathbb{N}$. 
	- I.E you prove that a function exists that converts every element of $A$ to a member of $\mathbb{N}$ 
## Example
You can prove that the set of even numbers is countable with $f: \mathbb{N} \rightarrow \mathbb{N}_e$ , $f(x) = 2n$ because every integer maps onto an even number when multiplied by two

# Grid method
- This is a method used for counting the positive rationals $\mathbb{Q}^+$
- Make an infinite matrix with rows $i$ and columns $j$
	- Each element should have a numerator $p$ and denominator $q$
	- $q$ increases as $i$ increases
	- $p$ increases as $j$ increases

![Infinite grid](https://www.homeschoolmath.net/teaching/images/rationals-countable.gif)

Algorithm:
- Skip duplicates
- List all $\frac{p}{q}$ such that $p + q = 2$
- Increment the right side of the equation
	- $p + q = 3$
	- $p+q=4$
	- ...

# Proving uncountability
- Assume that the set is countable and find a contradiction
- 