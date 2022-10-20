# Improper
$A \subseteq B$
$A$ is a subset of or equal to $B$

## Quirks
Formal definition:
$$A \subseteq B \equiv \forall x. (x \in A \rightarrow x \in B) $$
- This means that a set can be a subset of itself 

# Strict
$A \subset B$
$A$ is a subset of $B$

## Quirks
Formal definition:
$$A \subset B \equiv A \subseteq B \land \exists y.(y \in B \land y \notin A)$$
- This means that not all elements of $A$ are in $B$
- --> $|A| < |B|$
