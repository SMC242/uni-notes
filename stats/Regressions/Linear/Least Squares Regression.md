There are two ways to optimise a model for the least [[Residuals]]:

# Sum of residual magnitudes
Summing the magnitude of all residuals
$$|e_{1}| + |e_{2}| + |e_{3}| + ... + |e_{n}|$$

# Sum of squared residuals
The output of this formula grows rapidly as residuals increase. This is helpful because large residuals are a severe problem for the model
$$e_{1}^2 + e_{2}^2 + e_{3}^2 + ... + e_{n}^2$$

# Convention
The sum of squares is the conventional method

# Conditions
The requirements for a least squares line are:
- Data should have a linear trend
- Residuals should be normally distributed
- The variability should be constant
- Independence

# Calculating the lead squares line
- The gradient is found with $b_1 = \frac{s_y}{s_x}R$ where $s_x$ and $s_y$ are standard deviations
	- $m = b_1 = \beta_{1}$ in the form from [[Linear Regression#Models]]
- Take a point from the summary table in the form $(\bar{x}, \bar{y})$
![[Example Summary Table.png]]
- Substitute $b_1$, $\bar{x}$, and $\bar{y}$ into $y - y_0 = m \times (x - x_0)$
- Solve for $y$
## Using a least squares table
With these tables, the "Estimate" column gives $b_0$ and $b_1$. Either $R$ is given or it can be derived from $R^2$.
![[Least Squares Table 1.png]]