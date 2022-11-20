# Overview
Induction is a method of proving a statement ("theorem")
- The statement must be [[Inference#Universal|universally quantified]]
- It works by iterating over a set and proving that if the statement holds for 3 cases

# Method
1. Pick your set (E.G $\mathbb{Z}, \mathbb{N}$)
2. Pick your base case - a concrete case (E.G $0$) and prove that the theorem holds for it
	- There may be multiple base cases like $P(0), P(1), P(2), ..., P(k)$
3. Assume that the theorem holds for $n$, which implies the theorem holds for $n + 1$
	- Prove the implication $P(n) \rightarrow P(n + 1)$
	- This is called the _inductive step_

See also: [[Domains]]

# Types of induction
## Mathematical
- Operates on $\mathbb{N}$ and $\mathbb{Z^+}$
- Usually of the form $\forall n. P(n)$
	- $P(n)$ is a [[Propositional Logic|propositional function]]
	- It's called an inductive hypothesis
- Show that your **base case** is $true$, then use the **inductive step** to prove that any $n + 1$ not covered by the **base case** is also true

### Example
Base case: $P(0)$ is $true$
Inductive step: $P(n) \rightarrow P(n + 1)$ is $true$ for $n \ge 0$

## Strong
- Assume $\forall i. (i \leq n \rightarrow P(i))$
- Show that $P(n + 1)$ holds