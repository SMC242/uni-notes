[[Linear Regression]]s can be done with [[Categorical Variables]] too

# How?
Assign each category a numerical value. This is called an *indicator variable*. Substitute that value into the model like so:
$\hat{y} = \beta_0 + \beta_1 \times v$ where $v$ is the converted value. See [[Linear Regression#Models]] for how to use a model.

# Conditions
Conditions still have to be checked, but one check can be skipped sometimes: if the categorical variable only has 2 levels, it will be linear.

![[Least Squares Regression#Conditions | Conditions]]

# Reference levels
Reference levels are the default values. All other levels are measured against it.

This is why there is a missing level in the intercept table.

## Examples
### 2 levels
`bankruptcy` can either be `bankrupt`: 1, or `not`: 0
![[2 Level Categorical Table.png]]

### 3 levels
`income_ver` can be `not`: 0, `source_only`: 1, or `verified`: 2
![[3 Level Categorical Table.png]]