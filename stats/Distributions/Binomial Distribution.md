# Purpose
Similar to [[Geometric Distribution]] but different in one key way: it finds the number of successes in $n$ repetitions instead of the probability of *a* success

# Formula
This outputs the probability of getting $k$ successes when repeating the processes $n$ times
$$\binom{n}{k}p^k(1-p)^{n-k}$$
- $\mu = np$
- $\sigma^2=np(1-p)$

## Conditions
- Independence
- $n$ is static
- Binary result
- $p$ is the same for all trials

# Normal approximation
Use this when $n$ is large. It works because the binomial distribution is almost normal after a certain point

## Conditions
$\mu$ and $\sigma$ are greater than 10. See [[#Formula]]

## Issues
When examining a small range, this method becomes inaccurate. To fix the inaccuracy, the output region needs to be made wider. This is achieved by subtracting 0.5 from the lower bound and adding 0.5 to the upper bound

# Negative binomial distribution
Outputs the number of trials $n$ required to see $k$ successes. The [[Geometric Distribution]] is a special case of this - it stops at the first success whereas the NBD continues.

## Formula
$$P(k^{th}\textrm{ success on the n}^th \textrm{ trial}) = \binom{n-1}{k-1}p^k(1-p)^{n-k}$$

## Conditions
- Independence
- Binary outcomes
- $p$ is stable
- The last trial must be a success


