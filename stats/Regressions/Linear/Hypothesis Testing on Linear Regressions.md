[[T test]] can be performed on [[Linear Regression#Models | Linear Models]] to find out if there is truly a slope or not. If there is a slope, the model can be used to predict the response variable. If not, it is useless.

# Hypotheses
The [[Hypothesis Testing#Format|hypotheses]] for this test will take the following forms:
$H_0$: The true coefficient is 0
$H_A$: The true coefficient $\ne$ 0

# Testing
Simply use a modified version of the T formula (the null value is usually 0):
$$T = \frac{b_1 - \textrm{null value}}{SE}$$
Often, tables are given in the following format:
![[T Test Output.png]]
See [[Least Squares Regression#Using a least squares table]] for how to read this table.

# Confidence intervals
Make a [[Confidence Interval]] like usual, but use $b_1$ instead of $\bar{x}$.
$$b_1 \pm T^*_{df} \times SE$$
The conclusion will look like "95% confident that with each unit increase in $x$, $y$ can be predicted to decrease *on average* by 2 to 3.46 units".