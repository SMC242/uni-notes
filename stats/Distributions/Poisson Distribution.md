# Usage
Estimating the number of successes during a period of time. Questions about this are usually being given numbers to put into the formula

# Formula
Outputs the probability of seeing $k$ events within a period. It takes in a rate $\lambda$.
$$P(\textrm{observe k events}) = \frac{\lambda^k e^{-k}}{k!}$$
- $\mu = \lambda$
- $\sigma = \sqrt{\lambda}$


# Conditions
- Large $n$
- Independence
	- Can be violated if accounted for by using different rates