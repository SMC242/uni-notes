# Overview
A provably good metric for communicating probabilities. It's a ratio of "won't happen" to "will happen"

$$odds = \frac{1-p}{p}$$
where $p$ is a probability

Example:
$0.001 = 999:1$

>[!INFO] Human Perception
> Humans are very poor translating words to probabilities. Odds are better for communication
> ![Perception distribution](https://raw.githubusercontent.com/zonination/perceptions/master/joy1.png)

# Log-odds
AKA logit

$$logit(p) = \log\left( \frac{p}{1-p} \right)$$

- Used for describing extremely unlikely events
- Scales with the number of zeroes in $p$
	- More zeroes = larger logit

## Use cases
- Communicating small probabilities
- Reducing the impact of [[Floating Point Representation#Round-off error|numerical instability]] in calculations
	- Probabilities are in the range $0..1$
	- Repeatedly multiplying them causes [[Floating Point Representation#Exceptions|underflow]]
	- Instead, sum logits

# Likelihood
$$\mathcal{L}(x_{i)}= f_X(x_i)$$
- Built on top of the [[Probability Distribution#Probability functions|probability distribution]]

## Log-likelihood
- $\log(P(B | A))$
- Easier to work with than likelihood
- Positive values indicate greater likeness