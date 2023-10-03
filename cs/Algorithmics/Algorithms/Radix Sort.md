# Overview
Radix sort is a non-comparative sorting algorithm that operates by sorting elements based on their individual digits or characters, from the least significant to the most significant or vice versa. It distributes elements into buckets according to the values of specific digit positions and then collects them back in order.

# Properties
- [[Algorithm Analysis#Time complexities|Time complexity]]
  - Best case: $O(n * k)$
  - Worst case: $O(n * k)$
    - Where $n$ is the number of elements to be sorted.
    - $k$ is the maximum number of digits or characters in the values being sorted.
- Space complexity: $O(n + k)$
- [[Algorithm Strategies|Strategy type]]: Distribution sort
- Stable: Yes
- Recursive: Radix sort is typically implemented using an iterative approach. Recursive implementations are possible but less common.

# Use case
Radix sort is useful in the following scenarios:
- When sorting integers or strings with fixed-length keys.
- When the range of values to be sorted is limited.
- When stability in sorting order is required.

# Vanilla implementation
The vanilla implementation of radix sort involves the following sub-procedures:
1. Determine the maximum number of digits (or characters) among all the values.
2. Start with the least significant digit (or character) and move towards the most significant.
3. In each pass, distribute elements into buckets based on the value of the current digit.
4. Collect elements back in the order determined by the digit.
5. Repeat the above steps for all digits.
6. The array is now sorted.

For a code example, see [Link to code implementation](https://www.geeksforgeeks.org/heap-sort/).

# Variants
Radix sort does not have many specialized variants, as its core strategy revolves around distributing elements into buckets based on digit values. However, there may be specific optimizations or adaptations based on the data types being sorted or the radix (base) chosen for sorting, such as binary radix sort or decimal radix sort.
