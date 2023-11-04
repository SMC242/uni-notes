# Overview
- The broadcasting rules are used when applying operations to arrays of mismatching size

See first: [[Arrays]] and [[Tensor Ranks]] 

# Rules
Broadcasting can only happen one of the following is true:
a. The dimensions of the operands are compatible
b. One of the dimensions is `1`

## Compatibility
Two dimensions are compatible if one of the following is true:
a. They are equal
b. One of the dimensions is `1`

# Examples
```python
import numpy as np

a = np.array([1, 2, 3])  # (3,)
b = 2                    # Scalar
a + b                    # 2 is added to each element
```

- `b` implicitly takes on the shape of `a`