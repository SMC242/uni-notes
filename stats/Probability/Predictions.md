When given a limited set of outcomes, an expected value $E(X)$ can be reached. Each outcome must have a corresponding probability. 

# Expected Value
- What the random variable is expected to be
	- The true average (population mean)
- The weighted sum of each outcome (weighted by probability)

## Discrete
Multiply each possible outcome with its probability. Basically `zipWith (*) xs ps`
$$
\mathbb{E}(X) = x_1P(X = x_1) + x_kP(X=x_k)
$$

Using the [[Probability Distribution#Probability mass function|probability mass function]]:
$$\mathbb{E}(X) = \sum\limits_{x} x \cdot f_{X} (x)$$

## Continuous
$$\mathbb{E}(X) = \int_{x} x \cdot f_X(x) dx$$
where $f(x)$ is a [[Probability Distribution#Probability density function|probability density function]]

## Expected value of function
- You can get the expected value of a function by weighting it with the probability of the outcome
- This doesn't affect the probability, only the value
	- Useful for representing real-world rules like scoring systems (E.G rolling a 2 on a die gets 4 points)

Discrete:
$$\mathbb{E}\left(g(X)\right) = \sum\limits_x f_{X}(x) \cdot g(x) dx$$

Continuous:
$$\mathbb{E}\left(g(X)\right) = \int_{x} f_{X}(x) \cdot g(x) dx$$

> [!WARNING]
> $\mathbb{E}\left(g(X)\right) \ne g(\mathbb{E}(X))$


# Variance
This is the average distance of actual values from the expected value. It has 3 parameters:
- $x_k$ is the $k^{th}$ actual value
- $P(X=x_1k)$ is the probability that $X$ is $x_k$
- $\mu$ is the expected value (population mean) from [[#Expected Value|E(X)]]

$$
\sigma^2 = \sum_{j=1}^k (x_j-u)^2 P(X = x_j)
$$
The standard distribution $\sigma$ is the square root of the variance $\sigma^2$

Formula for random variables:
$$\sigma^{2} = \mathbb{E}\left((X - \mathbb{E}(X))^2\right)$$

# Predicting With Multiple variables
Multiply each expected value by a known coefficient.

These coefficients are grounded in the question. In the case of estimating total travel time each week, the coefficient for each day would be 1 because you're adding them all up. For a net gain calculation, some variables will be positive or negative
$$E(A) = a \times E(X) + b \times E(Y)$$
Variance is similar. 
$Var(aX+bY)=a^2\times Var(X)+b^2\times Var(Y)$
