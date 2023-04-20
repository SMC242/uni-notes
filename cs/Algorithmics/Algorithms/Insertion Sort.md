# Overview
Insertion Sort finds unsorted elements and places them into the correct position. This forms sorted (left) and unsorted (right) partitions of the array.
 
# Properties
Data about the algorithm such as:
- [[Algorithm Analysis#Time complexities|Time complexity]]
	- Best case: $O(n)$
	- Worst case: $O(n^2)$
- Space complexity: $O(n)$
- [[Algorithm Strategies|Strategy type]]: [[Algorithm Strategies#Incremental|incremental]]
- Stable? Yes
- Can be recursive or iterative

# Use case
- Small inputs
- Nearly-sorted inputs
	- Some algorithms sort until each partition is small and then employ Insertion Sort to finish sorting

# Vanilla implementation
- For each element, look to its left
	- Shift left until the left element is less than or equal to the current element
- Go to the next unsorted element and repeat until sorted

See [Link to code implementation](https://www.geeksforgeeks.org/insertion-sort/) for a code example
