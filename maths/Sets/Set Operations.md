# Set builder
$S = \{x | x \in U \land P(x)\}$
- AKA set comprehensions
- Read as "$x$ drawn from the set U where $x$ satisfies $P(x)$"
- $P(x)$ is a predicate, meaning that it converts its input to true or false

# Intersection
$A \cap B$
The elements that are in $A$ and $B$

# Union
$A \cup B$
The combination of $A$ and $B$

# Difference
$A \setminus B$
All elements in $A$ that are not in $B$

## Symmetric difference
$A \oplus B$
The complement of the union of $A$ and $B$ ($\overline{A \oplus B}$)

Can be simplified to $(A \setminus B) \cup (B \setminus A)$ which is where the name comes from

![[Subsets]]

# Empty set
$\emptyset$ or $\{\}$
The empty set

# The Universe
$U$ is the set of all things

# Cardinality
$|S|$
The number of elements in $S$

# Power set
$P(S)$
All subsets of S

## Size
The size of a power set is $2^n$ where $n$ is the cardinality of the original set

# Cartesian product
$A \times B$
Every element of $A$ tupled with every element of $B$

# Equality
Show that both sets are subsets of each other

Example proof:
$$
A = B \equiv \forall x. (x \in A \leftarrow \rightarrow x \in B)
$$
$$\forall x.((x \in A \rightarrow x \in B) \land (x \in B \rightarrow x \in A))$$
$$\forall x. (x \in A \rightarrow x \in B) \land \forall x.(x \in B \rightarrow x \in A)$$
$$A \subseteq B \land B \subseteq A$$
TL;DR: for all $x$, $x$ is in $A$ which implies that $x$ is in $B$ and vice versa

# Disjoint
$A \cap B = \emptyset \rightarrow disjoint(A, B)$

# Binary representation
Assign a bit to each member of $U$. Subsets of $U$ can then be represented as `1` if $x \in A$ else `0`

# Complement
$\overline{A}$ or $A^\complement$
All elements that aren't part of $A$
