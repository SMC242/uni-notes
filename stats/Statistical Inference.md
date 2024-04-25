# Overview
- Estimating the parameters of the global population based on some observed data (the samples)
- Assumes there is an underlying distribution

# Methods

## Maximum likelihood
AKA log-likelihood optimisation

- Optimise [[Odds#Log-odds|log-likelihood]] by changing the parameters of your model
	- Looking for the most negative log-likelihood


## Bayesian inference
- Guess the parameters
- Update the guesses based on [[Odds#Likelihood|likelihood]]
- Outputs distributions of parameters

> [!NOTE] Bayes' Rule
> $$P(A | B) \propto P(B | A) \cdot P(A)$$
> $\propto$ means "is proportional to"
>
> See also:
> - [[Conditional Probability]]
> - [[Conditional Probability#Bayes Theorem|Bayes Theorem]]


