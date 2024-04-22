# Overview
The probability of all [[Events And Outcomes|outcomes]] for a given experiment can be represented as a probability distribution $P(X = x)$. This notation can be read as "the probability that the outcome of experiment $X$ is $x$"

> [!TIP] Shorthands
> There are a few shorthands for $P(X = x)$
> - $P(X)$
> - $P(x)$  for a specific $x$

See also:
- [[Continuous Distributions]]

# Probability functions
- A [[Functions|function]] $f_{X}(x), x \rightarrow \mathbb{R}$ that defines the probability distribution
- There are two types depending on the nature of the random variable

| Condition           | Function name                      | $P(X = x)$ holds? |
| ------------------- | ---------------------------------- | ----------------- |
| Continuous function | Probability density function (PDF) | Yes               |
| Discrete function   | Probability mass function (PMF)    | No                |
## Probability density function
- Shows the spread of probability between the outcomes
	- The density at a given point
- Integrates to 1
	- $\int_{x} f_{X} (x) dx = 1$
	- But the maximum value of a PDF is not 1!
- The probability of any given value is 0
	- It's very unlikely that two observations will be equal because [[Domains#Number sets|real numbers]] are infinite

## Probability mass function
- Maps outcomes to probabilities (I.E retrieves the probability of an outcome)
- Sums to 1
	- $\sum\limits_{i} f_{X} (x_{i}) = 1$



> [!NOTE] Notation
> A special function notation is used here
> 
> - $f_X$ means that the function is for a random variable $X$
> - $f_{X}(x)$ means that it operates on a specific $x \in X$
> - $x \rightarrow \mathbb{R}$ is the [[Functions|function's domain and co-domain]] (as usual)

# Empirical distribution
- The distribution of a sample
- Won't be the same as the true distribution, but will tend towards the true distribution with repetitions
	- See [[Sampling#Central Limit Theorem|Central Limit Theorem]]
- Estimated from a set of values with $P(X = x) = \frac{n_{x}}{N}$
	- $n_x$ is the number of times $x$ was observed
	- $N$ is the number of trials
	- Approximates the [[#Probability mass function|PMF]]
		- Doesn't work for continuous values because the likelihood of the same value reappearing is low


# Distribution types
- The type of a distribution can be annotated like so: $X \sim U$ 
	- $U$ means that $X$ is uniformly distributed
