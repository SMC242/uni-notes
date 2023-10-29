See:
- [[Properties]] for what these rules mean
- [[Propositional Logic]] for operator definitions

# Identity
$P \land true \equiv P$
$P \lor false \equiv P$

# Domination
$P \lor true \equiv true$
$P \land false \equiv false$

# Indempotent
$P \land P \equiv P$
$P \lor P \equiv P$

# Double negation
$\lnot (\lnot P) \equiv P$

# Commutative
$P \land Q \equiv Q \land P$
$P \lor Q \equiv Q \lor P$

# Associative
$(P \land Q) \land R \equiv P \land (Q \land R)$
$(p \lor Q) \lor R \equiv P \lor (Q \lor R)$

# Distributive
$P \lor (Q \land R) \equiv (P \lor Q) \land (P \lor R)$
$P \land (Q \lor R) \equiv (P \land Q) \lor (P \land R)$

# De Morgan
$\lnot (P \land Q) \equiv \lnot P \lor \lnot Q$
$\lnot (P \lor Q) \equiv \lnot P \land \lnot Q$

# Contradiction
$P \land \lnot P \equiv false$
$P \lor \lnot P \equiv true$

# Implication
$P \rightarrow Q \equiv \lnot P \lor Q$

# XOR
$P \oplus Q \equiv (P \lor Q) \land \lnot (P \land Q)$

# Biconditional
$P \leftarrow \rightarrow Q \equiv (P \rightarrow Q) \land (Q \rightarrow P)$
