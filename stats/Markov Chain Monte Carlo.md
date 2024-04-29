---
aliases:
  - MCMC
---

# Overview
> [!DANGER] Challenges
> Computing [[Bayes' Theorem]] is difficult because:
> - $P(D | \theta)$ needs to be a distribution function instead of just a value
> - $P(D) = \int_{\theta} P(D | \theta) \cdot P(\theta)$ is often infeasible

- Built on top of [[Sampling#Monte Carlo|Monte Carlo]]
- Allows you to [[Sampling|sample]] from distributions that you can't sample from directly
	- Such as when you're doing [[Statistical Inference#Bayesian inference|Bayesian inference]]


> [!WARNING] Drawback
> Sampling strategy has a big influence on the result of MCMC

See first:
- [[Bayes' Theorem]]

See also:
- [[Statistical Inference#Bayesian inference|Bayesian inference]]

