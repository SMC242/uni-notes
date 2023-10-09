# Overview
This is an index of significant operations that can be applied to [[Arrays|NDArrays]]

# Concepts
## Rigid transformations
- A rigid transformation doesn't change the elements in memory, only the [[Array Representation#Header|headers]] --> $O(1)$ complexity

### List of rigid operations
- [[#Transpose]]
- [[#Rotate]]
- [[#Flip]]

## Rank promotion
Some operations change the [[Tensor Types|tensor rank]] of the array. They are sorted into three categories:
- Rank-preserving: the dimensions remain unchanged (E.G mapping, slicing all dimensions)
- Rank-reducing: reduces the number of dimensions (E.G reductions, unravelling)
- Rank-promoting: adds new dimensions

# Transpose
- Each column becomes a row, each row becomes a column
- Swaps the strides in the [[Array Representation#Dope vectors|dope vector]] --> $O(1)$
	- Does not affect the order of the elements in memory

```python
# Using NumPy
A.T
```

# Reshape
- Specify new dimensions and pour the elements into it like molten metal into a mould

```python
import numpy as np

x = np.arange(12)  # Dimensions: (12,)
x.reshape((3, 4))  # Dimensions: (3, 4)
```

## Rules
- The number of elements can't change
- The order of the elements in memory won't change, only the points where the array wraps to the next dimension
- The last dimension changes fastest, the second last changes slower, the third last changes slower still, etc

# Add singleton dimension
- If you have a vector, but need it to behave like a matrix, add a singleton dimension to promote its rank

```python
A = np.array([1, 2, 3])  # (3,)
A[np.newaxis, :]         # (1, 3)
A[:, np.newaxis]         # (3, 1)
```

This can also be done (in a less readable way) with `None` instead of `np.newaxis`

# Squeeze
- Removes [[#Add singleton dimension|singleton dimensions]]
- 