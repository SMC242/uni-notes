Hypothesis tests are a way of formally validating an assertion.

# Format
Two hypotheses are formed:
- Null hypothesis $H_0$
	- When the thing you're trying to prove is false
- Alternative hypothesis $H_A$
	- When it's correct
See [[#Drawing conclusions]] for why "trying to prove" isn't quite right

# Drawing conclusions
When writing the conclusion of the test, neither hypothesis is accepted. They can only be disproved. 

If there is sufficient evidence to support the alternative hypothesis, you write "rejected the null hypothesis". Otherwise, you have "failed to reject the null hypothesis".

# Degrees of freedom
Hypothesis tests take a parameter called "degrees of freedom" or $df$ which is used to adjust the test depending on how large the sample size is. A test using few DoF has to be far more stringent to ensure a [[Conclusion Errors#Type 1|type 1 error]] is not made.

# Level of significance
Level of significance or $\alpha$ is the probability of making a [[Conclusion Errors#Type 1|type 1 error]]. The default value is 0.05 (5%) due to tradition.