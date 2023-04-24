# Definition
- The average [[running time]]

# Method
There are three methods

## Aggregate
- Write down the running times of a series of calls to the function
- Add all of the running times together and divide by the number of operations
- Simplify

### Example
$$
\begin{aligned}
& O(1) + O(1) + O(1) + ... + O(n) = \frac{n \cdot O(1) + O(n)}{n} \\
&= O\left(\frac{n}{n}\right) \\
& = O(1)
\end{aligned}
$$

## Accounting
- Assign each operation a cost
- Uhhh something
- I can't find a good explanation

## Potential
- Also can't find a good explanation