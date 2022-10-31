# Overview
- Showing that from some premise $P$, some conclusion $Q$ holds
$P \rightarrow Q$

# Equivalence
To prove three statements are equivalent, prove them in a chain of implications
$$(P \rightarrow Q) \rightarrow (Q \rightarrow R) \rightarrow (R \rightarrow P)$$
```mermaid
graph
	P --> Q;
	 Q --> R;
	R --> P;
```

In order to do this, prove each group

Alternatively, use [[Inference#Hypothetical syllogism|Hypothetical syllogism]] to prove it like so:
$$\begin{align}
Q \rightarrow P \\
R \rightarrow Q \\
P \rightarrow R
\end{align}$$

# Via implication
![[Implication Proofs]]

# Contradiction
- Assume that the opposite of what you want to prove is valid and show that this is $false$

Given $P \rightarrow Q$, prove that $P \land \lnot Q$  is $true$

# By cases
- Find multiple premises such that $P \rightarrow (p_{1} \lor P_{2} \lor ... \lor P_{n})$
- Prove each case
	- $P_{1} \rightarrow Q$
	- $P_{2} \rightarrow Q$
	- $P_{n} \rightarrow Q$

# Induction
![[Induction Proofs]]