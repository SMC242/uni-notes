# Models
Linear regression is the fancy name for finding the line of best fit for a linear relationship
- A linear relationship between two variables involves a predictor $x$ and a response $y$
![[Linear Models.png]]
# Form
- Linear equations are of the form $y = \beta_{0} + \beta_{1}x + \epsilon$ where $\beta_{0}$ and $\beta_{1}$ are parameters and $\epsilon$ is the error
- The error is usually dropped because we usually want to predict the average response

# Non-linear relationships
Not all variables have a linear relationship, but that does not mean there is no relationship. The equation of the line of best fit may be quadratic or polynomial. Linear regressions are not useful in this case
![[Quadratic Dataset.png]]

# Predicting an outcome
Simply substitute $x$ into the model

# Understanding models
Take $y = 24 319 - 0.0431x$ as an example. When $x = 0$, the predicted response is $24319$ (the intercept). This is indicates a floor on the response variable; a minimum value for the response. The sign on the coefficient of $x$ shows an inverse relationship between $y$ and $x$

In many cases, the model is not valid for all values of $x$. Check this assumption before using the intercept.

#  Extrapolation
Extrapolation is when you use a model to predict responses outwith the range of the dataset. It's not a safe assumption that data that hasn't been modelled will behave the same as what has been modelled.

