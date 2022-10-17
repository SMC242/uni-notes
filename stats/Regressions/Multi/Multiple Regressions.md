Multi-regression is a way of handing multiple [[Linear Regression#Models|predictor variables]] when doing [[Linear Regression]]s. 

# Notation
Predictors: $x_1, x_2, x_3, ..., x_n$
Models: $\hat{y} = \beta_0 + \beta_1 x_1 + \beta_2 x_2 + ... + \beta_n x_n$

# Creating models
If given an intercept table, you read it as usual. $\beta_0$ is row 1 of the "Estimate" column, $\beta_1$ is row 2, $\beta_2$ is row 3, etc.
![[Least Squares Table 1.png]]

Multi-regressions are often used when using a [[Categorical Regressions|categorical predictor]] variable. Each level is represented in the model as an $x_i$ apart from the [[Categorical Regressions#Reference levels|reference level]]. Every $x_i$ will have its own coefficient $\beta_i$ . The combination of the two ($\beta_ix_i$) is called an "indicator function".

# Using models
## Categorical indicator variables
Substitute the value for the desired level into the equation and set all other indicator variables ($x_i$) to 0. 

Example: you want to predict the `rate` for a gnome where `gnome` = 1:
$$\hat{rate} = 1.2 + 1.8 \times product_{gnome} + 4.3 \times product_{hat}$$
$$\hat{rate} = 1.2 + 1.8 \times 1 + 4.3 \times 0$$

If predicting the reference level, set all of the indicator variables to 0.

# Counting predictors
Counting the number of coefficients ($\beta_i$) gives the "effective" number of predictors. A categorical predictor will have $n - 1$ terms in the model, where $n$ is the number of levels.

# Co-linearity
When predictor variables are correlated with each other. This complicates the relationship with the response variable.

Experiments can be designed to avoid this.

# Conditions
Multi-regression models should meet all of the following conditions:
- Residuals are nearly normal
- Variability of the residuals is nearly constant
- Residuals are independent
- The relationship between each variable and the outcome is linear
