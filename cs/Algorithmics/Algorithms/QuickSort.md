# Overview
QuickSort selects an element (the "*pivot*") and sorts relative to it. It recurses until it reaches a single-element slice. There are multiple strategies to pick that element

It has a bad worst-case complexity, but it is provably closer to the best case on average

# Properties
- [[Algorithm Analysis#Time complexities|Time complexity]]
	- Best case: $O(n \log n)$
	- Worst case: $O(n^2)$
- Space complexity: $O(\log n)$
- [[Algorithm Strategies|Strategy type]]: [[Algorithm Strategies#Divide and conquer|divide and conquer]]
- Stable? No
- Recursive? [[Recursion#Types#Binary|binary recursive]]

# Use case
- Great for large arrays
- The worst case is exploitable, so not as good for user-inputted data
	- A user can construct a worst-case input

# Vanilla implementation
- Has two functions: 
	- `quickSort(xs: T[], lower: int, upper: int)`: picks a pivot, then sorts the halves of the array around that pivot
	- `partition(xs: T[], lower: int, upper: int)`: selects a pivot and sorts a slice into elements less than and elements greater than the pivot. Returns the index of the pivot to `quickSort`
		- This means that the sides are only sorted *relative to* the pivot
		- `[2, 8, 3, 5, 9, 48, 13, 19]` where the pivot is `9`
		- Usually uses the [[#Right/left pivot]] strategy
- It will recurse until it reaches a list with 1 or less elements

See [Geeks For Geeks](https://www.geeksforgeeks.org/quick-sort/) for a code example

# Partitioning strategies
There are a few different ways to select the pivot

## Right/left pivot
- The start or end of the slice is selected as the pivot

## Median of 3
In `medianOf3(xs: T[], lower: int, upper: int)`:
- Look at the first, middle, and last elements in the slice
- Sort them into ascending order
- Pick the middle of the three as the pivot

In `partition(xs: T[], lower: int, upper: int`:
- Sort around the pivot

See [Java2S](http://www.java2s.com/Tutorial/Java/0140__Collections/Quicksortwithmedianofthreepartitioning.htm) for a code example

## 3-way
- A three-way quicksort partitions the slice into 3 sections:
	- `[lower..i]`: elements less than the pivot
	- `[i..j]:` elements equal to the pivot
	- `[j..upper]`: elements greater than the pivot
	- Where `i` is the index of the start of the pivot region and `j` is the end

## Random pivot
- Selects a pivot randomly
- This prevents exploits to create worst case scenarios *if and only if* a truly random number generator is used
- If a pseudo-random number generator is used, the [[Random Number Generators#Seeding|seed]] can be found and exploited
- This is slow either way because it takes a while to generate a number

See [[Random Number Generators]]

## Random

# Variants
## Hybrid
- Slices are sorted until they are of length `k`
- It then defers to [[Insertion Sort]] to avoid further recursion
- This is more memory efficient due to less stack frames being created
- Additionally, since [[Insertion Sort]] is fast for small arrays, it improves speed

![[QuickSelect]]