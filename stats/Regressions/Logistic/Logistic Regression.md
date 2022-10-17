This method is used when a model should output a categorical response with two levels. This is because [[Multiple Regressions]] don't work here.

# Notation
$Y_i$ is the outcome (response)
$i$ is the index of the observations

$x_{1,i}$ is the value of predictor 1 for observation $i$

A logistic regression model (LGM) looks like this: $$transformation(p_i) = \beta_0 + \beta_1 x_{1,i} + \beta_2 x_{2, i} + ... + \beta_k x_{k,i}$$
where $transformation$ is some function that converts from one scale to another. 


# Transformation functions
The $logit$ function is the most common function.
$$logit(p_i) = log_e(\frac{p_i}{1 - p_i})$$

You can solve for $p_i$ by applying $e$ to both sides of a model. It's easiest to evaluate RHS before applying $e$:
$$p_i = \frac{e^{model}}{1 + e^{model}}$$

You would do this if asked for the probability of the result, rather than the result. Remember that a logistic regression model outputs a choice of two outcomes, not a probability.

# Understanding the output
The reason a transformation function is used is to make the LHS grow proportionally with the RHS. The relationship between the LHS and RHS is defined by the transformation function. If a transformation function was not used, the output could only be between 0 and 1.

$p_i$ is the probability of the outcome taking a value of 1. For example, when predicting employment based on whether an applicant owns a car, $p_i$ would be the probability of them being employed.

# Significance
When [[Hypothesis Testing on Linear Regressions |hypothesis testing]] on LGMs, the normal distribution is used instead of the T distribution. In practical terms, this means the Z score is used instead of the T score.
![[LGM Summary Table.png]]

# Conditions
- The outcomes $Y_i$ are independent of each other
- Predictors $x_i$ are linearly related to the transformation function when the other predictors are constant