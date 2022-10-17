When doing [[Multiple Regressions]], you can end up with models that have many variables. Not all of these variables are influential and they can instead introduce noise.

# Jargon
<dl>
	<dt>Parsimonious model</dt>
	<dd>A model with superfluous variables removed</dd>
	<dt>Full model</dt>
	<dd>A model with all explanatory variables left in</dd>
</dl>

# $R^2$ check
A primitive pruning method is to remove a variable from the model and compare the $R^2_{adj}$ before and after. See [[Residuals#Adjusted R 2]] for details about this statistic

# Backward elimination
This is a better method. It attempts to delete each predictor and measure whether that increased the accuracy of the model using an [[#R 2 check]]. 

# Forward selection
The reverse of [[#Backward elimination]]. Add variables to the model and see if they increase the accuracy.

# Which method?
Backward elimination and forward selection can arrive at different models. It doesn't matter which one is used. If both are used, select the model that has the largest $R^2_{adj}$.

# Using p-values
You can do BE or FS by filtering for predictors with a p-value less than $\alpha$. 

## Is this better than $R^2_{adj}$?
It's less accurate, however it tells you how significant each predictor is.