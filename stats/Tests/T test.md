# What are they?
The Student's T-test is a way of formally checking if two populations have a statistically significant difference. You generate a T score, then compare it against the appropriate [[#critical value]] for your [[Hypothesis Testing#Degrees Of Freedom | degrees of freedom]].

# Formula
$$T = \frac{\bar{x} - \textrm{null value}}{SE}$$ where
$\bar{x}$ is a point estimate,
$\textrm{null value}$ is the value of $\mu$ if the null hypothesis is true,
and $SE$ is the [[#standard error]].

# Standard error
Standard error represents how wrong each estimate is expected to be.
## Formulae
Normal case:
$$SE = \frac{\sigma}{\sqrt{n}}$$
Difference of two means:
$$SE = \sqrt{\frac{\sigma_1^2}{n_1}+\frac{\sigma_2^2}{n_2}}$$

# Critical value
The T score must be smaller than the critical value (or "p value") in order to reject the null hypothesis. The p value is a function of the [[Hypothesis Testing#Degrees of freedom | degrees of freedom]]. For a T test, $df = n - 1$. Then look up the p value for your [[Hypothesis Testing#level of significance | level of significance]].
![[T Table.png]]
If you are using a [[#Tails|two-tailed test]], you must half your level of significance when looking up the p-value. This is because you are splitting your LoS between the two tails.

# Tails
A T test may have one or two "tails". This refers to whether it checks on one or both sides of the T distribution.
![[Tails Example.png]]