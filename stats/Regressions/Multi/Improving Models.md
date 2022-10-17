Nonlinear relationships, inconsistent variability, and "gaps" in the model are all issues that a model may have. Fortunately, there are methods to handle these flaws. This is useful when doing [[Linear Regression]]s, but mostly [[Multiple Regressions]]. 

# Variable transformations
A nonlinear relationship can be transformed such that it is linear. There are a few functions that may be applied to a predictor variable $x$:
- $log x$
- $\sqrt{x}$
- $\frac{1}{x}$
- Introducing a cap on the max value ("truncation")

Try each function to see what works best for the particular variable.

# Limitations
There are some values that each function is undefined for. Select a function that will always be defined for the data you have. If a variable will always be positive, $\sqrt{x}$ will work, while $logx$ and $\frac{1}{x}$ may not.

Here is a list of the limitations for each function:
| Function      | Undefined when |
| ------------- | -------------- |
| $logx$        | $x = 0$        |
| $\frac{1}{x}$ | $x = 0$        |
| $\sqrt{x}$    | $x < 0$        |
| Truncation    | None           |

# Reporting
Applying transformations also makes it harder to understand the model. It's important to state the issues that were solved with your transformations when reporting about the model.

A model doesn't have to be perfect to be useful. Be transparent with its flaws and avoid making claims past what the data can support.