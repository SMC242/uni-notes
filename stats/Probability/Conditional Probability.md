# Definition
When you calculate a probability given a condition. This usually means filtering the cases by a predicate such as value = 3. 

<dl>
	<dt>Marginal probability</dt>
	<dd>When a probability relies on one variable<dd>
	<dt>Joint probability</dt>
	<dd>When a probability relies on multiple variables</dd>
</dl>

# Probability table
These tables are used to summarise total probabilities.

![[probability_table.png]]

This is a second way of formatting the table. It does not function differently.
![[joint probability table.png]]

# Formula
You just take the probabilities and put them into the following equation:

$P(A|B) = \frac{P(A \cap B)}{P(B)}$

See also:
- [[Probabilistic  Models#Multiplication rule|AND in statistics]]

# Bayes Theorem
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