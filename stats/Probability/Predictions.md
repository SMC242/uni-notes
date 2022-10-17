When given a limited set of outcomes, an expected value $E(X)$ can be reached. Each outcome must have a corresponding proabability. 

# Calculating Expected Value
Multiply each possible outcome with its probability. Basically `zipWith (*) xs ps`
$$
E(X) = x_1P(X = x_1) + x_kP(X=x_k)
$$
# Variance
This is the average distance of actual values from the expected value. It has 3 parameters:
- $x_k$ is the $k^{th}$ actual value
- $P(X=x_1k)$ is the probability that $X$ is $x_k$
- $\mu$ is the expected value (population mean) from [[#Calculating Expected Value|E(X)]]

$$
\sigma^2 = \sum_{j=1}^k (x_j-u)^2 P(X = x_j)
$$
The standard distribution $\sigma$ is the square root of the variance $\sigma^2$


# Predicting With Multiple variables
Multiply each expected value by a known coefficient.

These coefficients are grounded in the question. In the case of estimating total travel time each week, the coefficient for each day would be 1 because you're adding them all up. For a net gain calculation, some variables will be positive or negative
$$E(A) = a \times E(X) + b \times E(Y)$$
Variance is similar. 
$Var(aX+bY)=a^2\times Var(X)+b^2\times Var(Y)$
