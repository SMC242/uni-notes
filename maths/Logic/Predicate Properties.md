See [[Predicate Logic]] for operator definitions

# Commutative
You can only apply these when the quantifiers are the same
$\forall x. \forall y. Q(x, y) \equiv \forall y. \forall x. Q(x, y)$
$\exists x. \exists y. Q(x, y) \equiv \exists y. \exists x. Q(x, y)$

# Negation
$\lnot (\exists x. \lnot P(x)) \equiv \forall x. P(x)$
$\lnot(\forall x. \lnot P(x)) \equiv \exists x. P(x)$

# Distributive
$\forall x. (P(x) \land Q(X)) \equiv (\forall x. P(x)) \land (\forall x. Q(x))$
$\exists x. (P(x) \lor Q(x)) \equiv ((\exists x. P(x)) \lor (\exists x. Q(x))$