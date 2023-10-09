---
tags:
  - ADT/Heap
---
# Overview
- This data structure implements the [[Heap]] [[Abstract Data Type|ADT]]
- A max-heap is a binary tree-based data structure where each node has a value that is greater than or equal to the values of its children. The maximum value is always at the root of the tree. Max-heaps are commonly used in [[priority queue]]s and various algorithms where efficient access to the largest element is required.

# Properties
## [[Time Complexity|Time complexities]] of major operations
- Insertion: $O(\log n)$
- Extraction (Deletion of Maximum): $O(\log n)$
- Peek (Maximum): $O(1)$

## Upsides:
- Efficient access to the maximum element.
- Useful in [[priority queue]] implementations and algorithms like Heap Sort.

## Downsides:
- Limited support for other types of queries (e.g., finding the k-th largest element).
- Some operations have logarithmic time complexity.

# Members

- Heap Type: Max-Heap (each node's value is greater than or equal to its children's values).
- Heap Size: The number of elements in the max-heap.
- Heap Array: The array or list used to store the elements of the heap.

# Operations

How each operation is implemented and their [[Time Complexity|time complexities]]

## Insertion

- Description: Adds a new element to the max-heap while maintaining the max-heap property.
- Time Complexity: $O(\log n)$

```
Procedure Insert(max_heap, value):
	Increment Heap Size
	Add the new value to the end of the heap array
	Heapify Up (restore the max-heap property by moving the value upwards)
```


## Extraction (Deletion of Maximum)

- Description: Removes and returns the maximum element from the max-heap while maintaining the max-heap property.
- Time Complexity: $O(\log n)$

```
Procedure ExtractMax(max_heap):
	Swap the root (maximum element) with the last element in the heap
	Decrement Heap Size
	Remove the last element from the heap array
	Heapify Down (restore the max-heap property by moving the new root downwards)
	Return the old root (maximum element)
```

## Peek (Maximum)
- Description: Returns the maximum element in the max-heap without removing it.
- Time Complexity: $O(1)$

```Function PeekMax(max_heap):
Return the root element (maximum element)
```

## Optional operations

- Build Max-Heap: Creates a max-heap from an array of elements.
- Increase Key: Modifies the value of a specific element and restores the max-heap property.
- Merge: Merges two max-heaps into one while maintaining the max-heap property.