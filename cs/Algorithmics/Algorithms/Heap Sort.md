# Overview
Heap sort is a comparison-based sorting algorithm that uses a binary heap data structure to build a max-heap or min-heap, and then repeatedly removes the top element from the heap to place it in its correct position.
# Properties
- [[Algorithm Analysis#Time complexities|Time complexity]]
	- Best case: $O(n \log n)$
	- Worst case: $O(n \log n)$
- Space complexity: $O(1)$ (in-place sorting)
- [[Algorithm Strategies|Strategy type]]: Comparison-based sorting
- Stable: No
- Recursive: No

# Use case
Heap sort is useful in the following scenarios:
- When you need an efficient, in-place sorting algorithm with a guaranteed worst-case time complexity.
- In embedded systems or applications with limited memory due to its small memory footprint.

# Vanilla implementation
Heap sort is implemented using the following sub-procedures:
1. **Build Heap**: Build a max-heap (for ascending order) or min-heap (for descending order) from the input array.
   - Starting from the middle of the array and moving backward, sift down each element to maintain the heap property.
2. **Heapify**: Repeatedly extract the root element (either the maximum or minimum) from the heap and place it at the end of the sorted portion of the array.
   - After each extraction, reduce the heap size and restore the heap property by sifting down the new root element.
3. Repeat step 2 until the entire array is sorted.

For a code example, see [Link to code implementation](https://www.geeksforgeeks.org/heap-sort/).

# Variants
Heap sort itself doesn't have many variants, but variations exist in the type of heap used (max-heap or min-heap) and the application of the algorithm, such as external sorting using a variation called "Polyphase merge heap sort."