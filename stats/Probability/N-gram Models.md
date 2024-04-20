# Overview
A type of model where you consider tuples of $n$ outcomes and consider the probability of them. Bigrams ($n = 2$) are the most useful so this note will focus on those, but will generalise to any $n$

- Use cases:
	- Computational linguistics
		- The principle behind [[Large Language Models|LLMs]]
	- 

See first:
- [[Joint Probability]]
- [[Conditional Probability]]

# Joint distribution
- $P(X_{i} = x_{i}, X_{i-1} = x_{i-1})$ is the normalised count of each pair

# Marginal distribution
- $P(X_{i} = x_{i})$ can be computed from the joint distribution with $\sum\limits_{i} c_{i-1}$
	- See [[Joint Probability#Marginal probability|Marginalisation]]

# Conditional distribution
$$P(X_{i} = x_{i} | X_{i-1} = x_{i-1}) = P\frac{X_{i}, X_{i-1} = x_{i-1}}{Count(P(X_{i-1} = x_{i-1}))}$$
- FIXME: possibly wrong
