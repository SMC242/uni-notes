# Overview
The probability that two events occur at the same time

- Notation: $P(X, Y)$
	- Shorthand for $P(X = x) \cap P(Y = y)$
	- See [[Probability Distribution]]

See also:
- [[Conditional Probability]]

# Marginal probability
- Finding a [[Probability Distribution]] from the joint probability
	- This is useful for finding the distribution for one of the two variables
	- Works for either variable
- Discrete: $P(X) = \sum\limits _{y} P(X, Y)$
- Continuous: $P(X) = \int_{y} P(X, Y) dy$
- If the two variables are [[Independence|independent]], you can just solve $P(X, Y) = P(X) \cdot P(Y)$

> [!INFO] Marginalisation
> This process of removing a variable from a joint distribution is called "marginalisation"

