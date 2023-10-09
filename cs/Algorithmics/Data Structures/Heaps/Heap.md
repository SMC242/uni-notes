---
tags:
  - ADT/Heap
---
# Definition
A heap is a specialized tree-based data structure that satisfies the heap property, which means that the value of each node is either greater than or less than (depending on whether it's a [[max-heap]] or [[min-heap]]) the values of its children. In a [[max-heap]], the maximum value is always at the root, and in a [[min-heap]], the minimum value is at the root. Heaps are commonly used to efficiently find and remove the maximum or minimum element in a collection, making them essential in [[priority queue]] implementations.

# Motivation
## Use case
You would use a heap data structure when you need to:
- Quickly find and remove the maximum or minimum element from a collection.
- Implement a [[priority queue]] for tasks like job scheduling or Dijkstra's algorithm for shortest path finding.
- Efficiently sort elements using Heap Sort, which has a time complexity of O(n log n).

# Comparison
Heaps are often compared to other data structures like arrays, linked lists, and binary search trees. Here's a comparison of their advantages and disadvantages in relation to these structures:

<ul class="breakdown">
	<li class="pro">Efficient for finding and maintaining the maximum or minimum element in a collection (max-heap and min-heap, respectively) in constant time (O(1)).</li>
	<li class="pro">Useful in [[priority queue]] implementations for tasks such as job scheduling and Dijkstra's algorithm for shortest path finding.</li>
	<li class="pro">Heaps can be efficiently used for sorting elements (Heap Sort) with a time complexity of O(n log n).</li>
	<li class="pro">Simple to implement and require less overhead compared to some other data structures.</li>
	<li class="con">Heaps do not support efficient search, deletion of arbitrary elements, or finding the k-th smallest or largest element (without additional data structures).</li>
	<li class="con">Heap operations (insertion and removal) can have an average-case time complexity of O(log n), which can be slower than some other data structures for certain tasks.</li>
	<li class="con">Implementing a [[max-heap]] or [[min-heap]] may require maintaining the heap property during insertion and removal, which can be complex for some applications.</li>
	<li class="con">Heaps may not be as versatile as other data structures like balanced binary search trees (e.g., AVL trees) in some scenarios.</li>
</ul>

# Members
The main attributes of a heap include:
- Heap Type: Whether it's a [[max-heap]] (where the maximum element is at the root) or a [[min-heap]] (where the minimum element is at the root).
- Heap Size: The number of elements in the heap.
- Heap Array: The array or list used to store the elements of the heap.

# Operations
Common operations defined for heap data structures include:

## Insertion
- Description: Adds a new element to the heap while maintaining the heap property.
- Time Complexity: O(log n), where n is the number of elements in the heap.

## Extraction (or Deletion)
- Description: Removes and returns the maximum element (in a [[max-heap]]) or the minimum element (in a [[min-heap]]) while maintaining the heap property.
- Time Complexity: O(log n), where n is the number of elements in the heap.

## Peek (or Top)
- Description: Returns the maximum element (in a [[max-heap]]) or the minimum element (in a [[min-heap]]) without removing it from the heap.
- Time Complexity: O(1).

## Optional operations
Additional operations that can be implemented for heap data structures include:
- Build Heap: Creates a heap from an array of elements.
- Decrease/Increase Key: Modifies the value of a specific element and restores the heap property.
- Merge: Merges two heaps into one while maintaining the heap property.

# Implementations
All of implementations of this [[Abstract Data Type|ADT]]:

```dataview
LIST
FROM
	#ADT/Heap
WHERE
	file.name != this.file.name
`````