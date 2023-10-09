---
tags:
  - ADT/Heap
---
# Overview
- This data structure implements the [[Heap]] [[Abstract Data Type|ADT]]
- A min-heap is a specialized binary tree-based data structure where each node has a value that is less than or equal to the values of its children. The minimum value is always at the root of the tree. Min-heaps are commonly used in [[priority queue]]s and various algorithms where efficient access to the smallest element is required.

# Properties
[[Time Complexity|Time complexities]] of major operations:
- Insertion: $O(\log n)$
- Extraction (Deletion of Minimum): $O(\log n)$
- Peek (Minimum): $O(1)$

Upsides:
- Efficient access to the minimum element.
- Useful in [[priority queue]] implementations and algorithms like Dijkstra's shortest path.

Downsides:
- Limited support for other types of queries (e.g., finding the k-th smallest element).
- Some operations have logarithmic time complexity.

# Members
- Heap Type: Min-Heap (each node's value is less than or equal to its children's values).
- Heap Size: The number of elements in the min-heap.
- Heap Array: The array or list used to store the elements of the heap.

# Operations
## Insertion
- Description: Adds a new element to the min-heap while maintaining the min-heap property.
- Time Complexity: $O(\log n)$

```
Procedure Insert(min_heap, value):
    Increment Heap Size
    Add the new value to the end of the heap array
    Heapify Up (restore the min-heap property by moving the value upwards)
```

## Extraction (Deletion of Minimum)
- Description: Removes and returns the minimum element from the min-heap while maintaining the min-heap property.
- Time Complexity: $O(\log n)$

```
Procedure ExtractMin(min_heap):
    Swap the root (minimum element) with the last element in the heap
    Decrement Heap Size
    Remove the last element from the heap array
    Heapify Down (restore the min-heap property by moving the new root downwards)
    Return the old root (minimum element)
```

## Peek (Minimum)
- Description: Returns the minimum element in the min-heap without removing it.
- Time Complexity: $O(1)$

```
Function PeekMin(min_heap):
    Return the root element (minimum element)
```

## Optional operations
- Build Min-Heap: Creates a min-heap from an array of elements.
- Decrease Key: Modifies the value of a specific element and restores the min-heap property.
- Merge: Merges two min-heaps into one while maintaining the min-heap property.