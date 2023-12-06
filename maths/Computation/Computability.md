# Overview
Not all problems have an algorithm that can reliably compute an answer. These problems are known as "unsolvable"

# Undecidable problems
- A decision problem $\Pi$ that has no algorithm is undecidable

# The Halting Problem
![[Halting Problem]]

# Reductions
If you can reduce all instances of $I_{1}\in \Pi_1$ to $I_{2} \in \Pi_2$ such that:
- $I_1$ is "yes" instance if $I_2$ is a "yes" instance

If a problem $\Pi_1$ is undecidable and there exists a reduction from $\Pi_1$ to $\Pi_2$, $\Pi_2$ is also undecidable