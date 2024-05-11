# Definition
When you calculate a probability given a condition. This usually means filtering the cases by a predicate such as value = 3. 

See also:
- [[Joint Probability]]
- [[Joint Probability#Marginal probability|Marginal probability]]

# Probability table
These tables are used to summarise total probabilities.

![[probability_table.png]]

This is a second way of formatting the table. It does not function differently.
![[joint probability table.png]]

# Formula
You just take the probabilities and put them into the following equation:

$P(A|B) = \frac{P(A \cap B)}{P(B)}$

See also:
- [[Probabilistic Models#Multiplication rule|AND in statistics]]

# Reconstructing prior
- You can get the prior $P(A)$ back by dividing $P(A | B)$ by all possible probabilities

# Bayes Theorem
![[Bayes' Theorem]]