# Overview
- Estimating the parameters of the global population based on some observed data (the samples)
- Assumes there is an underlying distribution

# Initial model
- [[Linear Regression]] can be used as a starting point for inference
- Assumption: each variable is just another variable with some scaling and shifting
- Higher dimensional form: $y = Ax + b$
	- See [[Matrices]]
- $y$ is a random variable, $x$ is known, $m, c, \sigma$ are parameters
	- Observations are of the form $(x_{1}, y_{1})$
- You can't actually use them for inference because linear regression just gives you a line for the observed data, not the unseen data
	- You could assume that this model works for the unseen data, but you'll be wrong

## Modelling epsilon
- A noise term $\epsilon$ can be added to account for real noise
- You need to assume the distribution of this term
	- Assuming that $\epsilon$ is [[Normal Distribution|normally-distributed]]: $y = mx + c + \mathcal{N}(0, \sigma^2)$
	- Using $mx + c$ as the mean: $y \sim \mathcal{N}(mx + c, \sigma^2)$

# Methods

## Direct estimation
- Defining [[Functions]] of observations to estimate the parameters
	- Requires estimator functions for each distribution
- You have to assume the distribution (E.G that it is [[Normal Distribution|normally distributed]])
- Efficient

## Maximum likelihood
AKA log-likelihood optimisation

- Start with a model
- [[Mathematical Optimisation|Optimise]] [[Odds#Log-odds|log-likelihood]] by changing the parameters of the model
	- Looking for the most negative log-likelihood
- Works in situations where you know the likelihood function


## Bayesian inference
- Uses distributions over parameters instead of direct values
- Start with initial guesses for the parameters (the initial hypotheses or "prior"s)
- Consider the parameters as random variables
	- This means we can estimate the distribution of their values
- Update the parameters based on the [[Odds#Likelihood|likelihood]] of a parameter taking on a value
	- Observations are used to create a tighter distribution (the "posterior")
	- The distribution would change as more observations are added to the dataset

Requirements:
- A likelihood function
- The ability to make good priors

See also:
- [[Bayes' Rule]]