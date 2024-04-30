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
>
> - [[Mathematical Optimisation#Linear least squares|Least squares]]
> - [[Matrix Operations#Pseudo-inverse|Pseudo-inverse]]

### Estimators

- Functions that take in a set of observations and return the estimated parameters
- You have to assume the distribution (E.G that it is [[Normal Distribution|normally distributed]])
  - This is called the model
- Must be created for each problem

> [!EXAMPLE] Common Estimators
>
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

### Mixture models

- When there are multiple components in a distribution
  - Example: a distribution with two [[Normal Distribution|normal-like]] humps
  - Used to model clusters
- Use multiple models $\mathcal{N}_i(\mu_{i}, \sigma_{i})$ with weighting factors $\lambda_i$
  - The weighting factors must add up to 1
  - Indicates the importance of a component (I.E how likely an observation is to fall in that component)
- Problem: no estimators exist for these models
- Solution: just optimise
  - The likelihood function is $\mathcal{L}(\theta | x)$ and is the log of the weighted sum of the [[Probability Distribution#Probability density function|PDFs]]
  - The parameter vector made up of all the $\mu, \sigma, \lambda$s

## Bayesian inference

- Uses distributions over parameters instead of direct values
	- Outputs the _models that are compatible with the data_ rather than a single best-fit model
- Start with initial guesses for the parameters (the initial hypotheses or "prior"s)
- Consider the parameters as random variables
	- This means we can estimate the distribution of their values
- Update the parameters based on the [[Odds#Likelihood|likelihood]] of a parameter taking on a value
  - Observations are used to create a tighter distribution (the "posterior")
	- The distribution would change as more observations are added to the dataset
- Posterior formula: $P(\theta|D) = \frac{P(D | \theta) \cdot P(\theta)}{P(D)}$ where $D$ is the data, $P(\theta)$ is a prior over parameters
- The predictive posterior is the expected distribution of observations
	- If the model is good, sampling from this will be like sampling from the real distribution
	- I.E it approximates the real distribution

Requirements:
- A prior $P(\theta)$ over a parameter vector
	- I.E an [[#initial model]] including values for $m$, $c$, and $\epsilon$
- A likelihood function $P(D | \theta)$
- A way to combine them into the posterior formula
	- $P(D)$ is usually infeasible to compute
		- Instead, approximate using [[Sampling#Monte Carlo|Monte Carlo]]
		- Slow to compute, even with MCMC

See also:

- [[Bayes' Rule]]

> [!TIP]
> Bayesian inference is the process of updating your prior beliefs based on new evidence

### Models as graphs

AKA graphical models

- Expressions can be converted to graphs
- Used to model dependencies between random variables
  - Some variables will have been observed, others won'will be unobserved
  - Dependencies are either deterministic (defined by the parent) or stochastic (defined by a distribution)
    - Stochastic nodes require prior distributions
- You can do inference on the graph
  - You can find the posterior distribution of the unobserved nodes (I.E infer their distribution) by integrating over the possible values given the observed values (the evidence)
- Variables can be fixed (assigned a fixed value) if you can express this knowledge as a distribution
- Stochastic nodes that don't depend on a deterministic node are not allowed
  - Such nodes are known as "wild nodes"

> [!EXAMPLE] > $$y = mx + c$$
>
> ```mermaid
> graph RL
> 	y
> 	m
> 	x
> 	mx
> 	c
> 	m --> mx
> 	x --> mx
> 	mx --> y
> 	c --> y
> ```

> [!EXAMPLE] Detailed Example
>
> $$
> \begin{align*}\\
> y &\sim N(\mu, \sigma)\\
> \mu &= mx + c
> \end{align*}
> $$
>
> ```mermaid
> graph RL
> 	y(y)
> 	m(m)
> 	x[x]
> 	mx(mx)
> 	c(c)
>     μ(μ)
>     σ(σ)
>
> 	m -.-> mx
> 	x -.-> mx
> 	mx -.-> μ
> 	c -.-> μ
>     μ --> y
>     σ --> y
> ```
>
> - Dotted lines are deterministic dependencies
> - Solid lines are stochastic dependencies
> - Circular nodes are random variables
> - Square nodes are observed variables
> - Key assumption: there is some invisible structure that can be modelled as a linear relationship to $x$

### Computing

- Computed using [[Markov Chain Monte Carlo]]
- We assume that $P(\theta | D) = \frac{P(D | \theta) P(\theta)}{\int_{\theta} P(D | \theta) P(\theta)} \propto P(D | \theta) P(\theta)$
  - I.E that $P(D)$ is just a normalising constant
- Make assumptions about the distributions for your stochastic variables (E.G $m, c, \sigma$)
- Find a likelihood function
  - For a linear regression, it's $L(D | \theta) = L(x, y; m, c, \sigma) = f_{X} (y - mx + c, \sigma^2)$
- The histogram of posterior samples is called the "trace"
-
