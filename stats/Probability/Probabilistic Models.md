# Overview
Models built on top of probability. Imagine a process determined by some variables ("unobserved variables"). You don't know what they are, but you can use inference to find out

There are a few classes of questions that you can ask about a probabilistic model:
- [[Conditional Probability|Probability given a previous outcome]]: forward probability
	- Questions about the distribution of *observations*
- Asking the distribution: inverse probability
	- Questions about the *unobserved variables* that led to the observations

# Multiplication rule
- Used in cases where you want to know the likelihood of multiple [[Independence|independent]] events occurring at the same time, such as rolling 3 on a die three times.
- If they're dependent, use [[Joint Probability]]

$$P(A \cap B) = P(A) \times P(B)$$

# Sum rule
$$P(A \cup B) = P(A) + P(B) - P(A \cap B$$

- The rule for finding the probability of either event happening
