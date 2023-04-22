# Overview
Repeatedly split the input into sub-arrays, sort them, and recombine them into a sorted array

# Properties
- [[Algorithm Analysis#Time complexities|Time complexity]]
	- Best case: $O(n \log n)$
	- Worst case: $O(n \log n)$
- Space complexity: $O(n)$
	- Stack frames can be very expensive in some languages
- [[Algorithm Strategies|Strategy type]]: [[Algorithm Strategies#Divide and conquer|divide and conquer]]
- Stable? Yes
- Recursive? [[Recursion#Binary|Binary recursive]]

# Use case
- Best for large inputs
- Sorting linked lists
- When you know that the array is unlikely to be sorted or nearly-sorted
	- Merge Sort will do redundant operations on a sorted list
-  When stability matters

# Vanilla implementation
See [Geeks For Geeks](https://www.geeksforgeeks.org/merge-sort/) for a code example

## Sort
Base case: `l >= r` (there is only one element, therefore the slice is sorted)

- Find the mid-point of the slice being considered
- Half the slice and recurse on each sub-array
- Call `merge` on the halves

[[Time Complexity]]: $O(\log_{2} n)$ (because the array is being split in half)

## Merge
This function sorts *__in place__*

- Copy each sub-array (left slice: `[lower..mid]`, right slice: `[mid..upper]`)
- A sentinel value (usually the largest integer that the language supports) is added to the end of each sub-array
	- This avoids having to check that an end of a sub-array has been reached
	- It works by giving the sub-array an extra element, so that the end is never reached. The merging loop will terminate before these elements are checked
- Combine them
	- Iterate with `i` (the index of the left slice) and `j` (the index of the right slice)
	- Compare `L[i]` and `R[j]`
	- Add whichever is smaller to the result array and increment the corresponding index

[[Time Complexity]]: $O(n)$

# Variants
## In-place
- Improves memory consumption by not copying the sub-arrays in `merge`
- Loses the stability property

## Bottom-up
- Iterative version
- Do the merges in pairs of 2 until the whole array has been merged

## Insertion sort
- Defer to [[Insertion Sort]] for small sub-arrays
- This usually happens when the sub-array has between 5 and 20 elements