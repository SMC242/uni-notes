This file contains methods for making sure that the [[Multiple Regressions#Conditions|conditions]] hold when doing [[Multiple Regressions]].

# Normality
Plot the residuals as a histogram and look for outliers.
![[Residual Histogram.png]]

# Variance
To check if the variance is normal, plot $|residual|$  against the predicted values. The points should appear in a cloud.
![[Variance Plot.png]]

# Independence
One way that a model could fail the independence condition is if a variable depends on another variable. For example, the time that an observation was taken may affect its value. This can be detected by plotting residuals against time.
![[Residual-time Plot.png]]

# Variability per variable
Check that the variability doesn't change much when each variable is used. This can be done by plotting residuals for the model with each variable on its own.
Box plots can be used to visualise residuals per level of a categorical variable
![[Residual Box Plot.png]]
