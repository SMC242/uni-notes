# Overview
Used to flip conditional probabilities from $P(\textrm{A is x | B is y})$ to $P(\textrm{B is y| A is x})$

$$
P(B|A) = \frac{P(B|A_1)P(A_1)}{P(B|A_1)+P(B|A_2)P(A_2)}
$$
This can be applied for as many variables as you need. Just add another $P(B|A_k)P(A_k)$ to the denominator.

- $P(A | B)$: the posterior
	- What we want to know
- $P(B | A)$: the likelihood
	- The likelihood of $A$ producing the observed evidence
- $P(A)$: the prior
	- The likelihood of $A$ regardless of the evidence
- $P(B)$: the evidence
	- The likelihood of $B$ regardless of the event

Alternative phrasing:
$$P(H | D) = \frac{P(D | H)P(H)}{P(D)}$$
where $H$ is the hypothesis and $D$ is some observed data


## Probability of D
$$P(D) = \sum\limits P(D|H) P(D)$$
- This is the formula for the denominator of Bayes' Theorem
- Usually intractable
- See [[Statistical Inference#Bayesian inference|Markov Chain Monte Carlo]] for how this is approximated

i) 0.6
ii)
==P(plastic) = 1/300==

Calculating these in case they're useful later:
P(gravel) = 1/100
P(irrigator) = 1/1000

iii)
 P(gravel|improve) = P(improve|gravel) P(gravel) / P(D)
 = (0.6 x 1/100) / P(D)
P(D) = P(improve|gravel) x P(gravel) + P(improve|plastic) x P(plastic) + P(improve | irrigator) x P(irrigator) = 0.6 x 1/100 + 0.3 x 1/300 + 0.9 x 1/1000

P(gravel|improve) = (0.6 x 1/100) / (0.6 x 1/100 + 0.3 x 1/300 + 0.9 x 1/1000)