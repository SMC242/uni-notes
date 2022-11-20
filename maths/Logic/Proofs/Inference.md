# Overview
Inference is the process of proving a statement. There are certain laws that you can use to prove things

Used for [[Proofs]]

# Premise notation
$$
\begin{aligned}
p \\
p \rightarrow q 
\\---\\
q
\end{aligned}
$$
This means that the conclusion from the premises $p$ and $p \rightarrow q$ is $q$

- These can be stacked into [[Proof Trees]] 

# Modus ponens
$(p \land (p \rightarrow q)) \rightarrow q$

# Modus tollens
$(\lnot q \land (p \rightarrow q)) \rightarrow \lnot p$

# Hypothetical syllogism
$((p \rightarrow q) \land (q \rightarrow r)) \rightarrow (p \rightarrow r)$

Think of this like a path
`A --> B --> C` can be shortened to `A --> C`
$p$ passes through a common node $q$ to reach $r$, so $p$ goes to $r$

# Disjunctive syllogism
$((p \lor q) \land \lnot p) \rightarrow q$

# OR
$p \rightarrow (p \lor q)$

# AND
$((p) \land (q)) \rightarrow (p \land q)$

# Simplification
$(p \land q) \rightarrow p$

# Quantifiers
## Universal
### Instantiation
$$
\begin{align}
\forall x \in U. P(x)
\\------------\\
\textrm{P(c) for an arbitrary c} \in U
\end{align}
$$
- Holds for any $x$

### Generalisation
This is [[#Quantifiers#Universal#Instantiation|universal instantiation]] flipped
$$
\begin{align}
\textrm{P(c) for an arbitrary c} \in U
\\------------\\
\forall x \in U. P(x)
\end{align}
$$

## Existential
### Instantiation
$$
\begin{align}
\exists x \in U. P(x)
\\-------------\\
\textrm{P(c) for some element c} \in U
\end{align}
$$
- Holds for some $x$

### Generalisation
This is [[#Quantifiers#Existential#Instantiation|existential instantiation]] flipped
$$
\begin{align}
\textrm{P(c) for some element c} \in U
\\-------------\\
\exists x \in U. P(x)
\end{align}
$$