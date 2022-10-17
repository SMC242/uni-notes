# Evaluating a model
A good way to see if a [[Linear Regression]] model is accurate is to look at how far actual values are from the values predicted by the model.

A residual is exactly that: $y - \hat{y}$ where $\hat{y}$ is the predicted value. Residuals are often compared by their magnitude ($|y - \hat{y}|$).

# Interpreting residuals
When reading residuals, think of them in reverse. A  ==positive== residual means the prediction is an ==underestimate== and a negative residual means it is an overestimate.

# Residual Plot
A tool for analysing residuals.
![[Residual Plot.png]]

# R value
A summary statistic for residuals (also referred to as "correlation of ..."). R values range between 1 and -1, where 1 is positively linear and -1 is negatively linear
![[R Value Examples.png]]

# R-squared
$R^2$ describes how close the data clusters to the line of best fit. This is used to evaluate how much variation is explained by the [[Least Squares Regression | Least Squares Line]].

A frequent question about this is "How much of the variation is explained by the explanatory variable?". 

## Formula
$$R^2 = 1 - \frac{\textrm{variablity in residuals}}{\textrm{variablity in outcome}}$$
Short form: $$R^2 = 1 - \frac{Var(e_i)}{Var(y_i)}$$
where $e_i$ are the residuals and $y_i$ are the outcomes.

## See also
[Khan Academy](https://www.khanacademy.org/math/ap-statistics/bivariate-data-ap/assessing-fit-least-squares-regression/a/r-squared-intuition)

# Adjusted $R^2$
When doing [[Multiple Regressions]], the normal $R^2$ formula isn't accurate enough. The adjusted formula fixes this by calculating degrees of freedom differently. $df = n - k -1$ in this context. Without this adjustment, the $R^2$ value would be too high, leading to false confidence that the model handles more variance than it really does.

## Adjusted formula
$$R^2_{adj} = 1 - \frac{\frac{s^2_{residuals}}{n - k - 1}}{\frac{s^2_{outcome}}{n - 1}}$$
More readable form: $$R^2_{adj} = 1 - \frac{s^2_{residuals}}{s^2_{outcome}} \times \frac{n - 1}{n - k - 1}$$
where $n$ is the number of cases,
$k$ is the number of predictors. 

Be careful when calculating $k$ if the model has categorical variables
![[Multiple Regressions#Counting predictors]]

# Residual-fit plot
This is a plot of estimated values $\hat{y}$ against the residuals. Useful when [[Checking Conditions]]
![Residual-fit plot](https://online.stat.psu.edu/stat462/sites/onlinecourses.science.psu.edu.stat462/files/04slrmodel/regress_alc_arm_resid02/index.png)