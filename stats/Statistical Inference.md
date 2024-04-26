# Overview
- Estimating the parameters of the global population based on some observed data (the samples)
- Assumes there is an underlying distribution

See first:
- [[Probability Distribution]]

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
	- Requires [[#Estimators|estimator functions]] for each distribution
- Efficient

> [!EXAMPLE] Examples
> - [[Mathematical Optimisation#Linear least squares|Least squares]]
> - [[Matrix Operations#Pseudo-inverse|Pseudo-inverse]]

### Estimators
- Functions that take in a set of observations and return the estimated parameters
- You have to assume the distribution (E.G that it is [[Normal Distribution|normally distributed]])
	- This is called the model
- Must be created for each problem

> [!EXAMPLE] Common Estimators
> - Arithmetic mean
> - Sample mean
> - Variance
> - Standard deviation

## Maximum likelihood
AKA log-likelihood optimisation, MLE

- Start with a model
- [[Mathematical Optimisation|Optimise]] [[Odds#Log-odds|log-likelihood]] by changing the parameters of the model
	- Likelihood: $\mathcal{L}(x_{1}, \dots, x_{n}) = \prod_{i} f_{X} (x_i)$
		- [[Independence]] required
	- Log-likelihood: $\log \mathcal{L}(x_{1}, \dots, x_{n}) = \sum\limits_{i} \log f_{X} (x_{i})$
	- The parameters are packed into $\theta$
		- These are what we're trying to optimise
- Works in situations where you know the likelihood function
- Used if you don't have an [[#Estimators|estimator]] for a parameter
- Notation for likelihood that depends on some parameters in a distribution $\theta$: $\mathcal{L}(\theta|x)$
- Objective function: $L(\theta) = - \log \mathcal{L}(\theta | x_{1}, \dots, x_{n} ) = - \sum\limits_{i} \log f_{X}(x_{i}; \theta)$
	- Aiming to minimise the negative log-likelihood avoids [[Floating Point Representation#Exceptions|underflow errors]]
	- [[Higher Order Optimisation#Gradient descent|Gradient descent]] can be used if this is differentiable

## Bayesian inference
- Uses distributions over parameters instead of direct values
- Start with initial guesses for the parameters (the initial hypotheses or "prior"s)
- Consider the parameters as random variables
	- This means we can estimate the distribution of their values
- Update the parameters based on the [[Odds#Likelihood|likelihood]] of a parameter taking on a value
	- Observations are used to create a tighter distribution (the "posterior")
	- The distribution would change as more observations are added to the dataset
- Posterior formula: $P(\theta|D) = \frac{P(D | \theta) \cdot P(\theta)}{P(D)}$ where $D$ is the data, $P(\theta)$ is a prior over parameters

Requirements:
- A prior $P(\theta)$ over the parameters
	- I.E an [[#initial model]] including values for $m$, $c$, and $\epsilon$
- A likelihood function $P(D | \theta)$
- A way to combine them into the posterior formula
	- $P(D)$ is usually infeasible to compute
	- Instead, approximate using [[Sampling#Monte Carlo|Monte Carlo]]

See also:
- [[Bayes' Rule]]

### Bayesian linear regression
- The parameters are packed into a [[Vectors|vector]] $\theta$
- 