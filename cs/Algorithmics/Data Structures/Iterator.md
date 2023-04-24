# Definition
An iterator is a [[Lazy Evaluation|lazy]] structure that fetches the next element until the end is reached.

# Motivation
## Use case
- When a collection is too large to hold in memory
- When you don't need the whole collection at once

# Comparison
## Iterator
<ul class="breakdown">
	<li class="pro">Only holds the current value in memory</li>
	<li class="con">You can't access other elements, which may be required for some algorithms</li>
</ul>

# Members
- `cursor`: the current element. Used to compute the next element

# Operations
The operations that are defined for this structure and their [[Time Complexity|time complexities]]

## Next
- Computes and returns the next element. Throws an error if there are no more elements
- Typically, a language will have a `for each x in xs` construct that will exit the loop when the end is reached
- [[Time Complexity]]: $O(f(x))$ where $f(x)$ is some function that gets the next element

```
function next(Iterator) -> T:
	if end_reached:  throw StopIteration()
	// Compute next element
	Iterator.cursor = x
	return x
```