# Overview
- Uses predicates instead of statements
- A predicate is a [[Functions|function]] that maps a value to `true` or `false`
	- $P: U \rightarrow \{true, false\}$

See [[Predicate Properties]] for identities

# Quantifiers
A modifier that can be applied to the values used in a statement

## Universal
- "forall"
- $\forall x. P(x)$
- This means that $P(x)$ is `true` for all values of $x$

## Existential
- "there exists some"
- $\exists x. P(x)$
- This means that $P(x)$ is `true` for some values of $x$

## Domain of discourse
When using quantifiers, you should be specific about what [[Domains|domain]] $x$ belongs to

$\forall x \in U. P(x)$ means that $P(x)$ is `true` for all values of $x$
whereas
$\exists x \in \mathbb{Z}. P(x)$ means that $P(x)$ is `true` for some integers

## Scope
Parentheses are used to indicate which variables the quantifier applies to

$\forall x. \exists y. (R(y, x) \land Q(x))$
The two quantifiers apply to all $x$es and $y$s

> [!WARNING]
> Order matters!
> $\forall x. \exists y. (R(y, x))$ means "for all $x$, there exists a $y$ where $R(y, x$) is `true`"
> 
> However, $\exists y. \forall x. (R(y, x))$ means "there exists a $y$ such that for all $x$, $R(y, x)$ is `true`"

