# Overview
- Statements that are `true` or `false` composed together
- These statements can be manipulated and simplified

See [[Propositional Logic Properties]] for the identities

# Symbols
## NOT
- "negation"
- $\lnot p$
- The opposite of $p$

## AND
- "conjunction"
- $p \land q$
- `true` if both $p$  and $q$ are `true`

## OR
- "disjunction"
- $p \lor q$
- `true` if either $p$ or $q$ are `true`

# XOR
- "exclusive or"
- $p \oplus q$
- `true` if either are `true` but not both

## Implication
- "conditional"
- $p \rightarrow q$
- See [[#Implications]]

## Biconditional
- $p \leftarrow \rightarrow q$
- See [[#Biconditionals]]

# Tautologies
- A statement that is always true
- Cannot be untrue
- They're usually specific, rather than universal statements

## Examples
- The ball is green
- $x = y$

## Not examples
- All balls are green
- Every value of $x$ is equal to $y$

# Implications
- The logical equivalent of "if-then-else" in programming
- States that the second statement always follows from the first

## Truth table
| $p$ | $q$ | $z$ |
| --- | --- | --- |
| 0   | 0   | 1   |
| 0   | 1   | 1   |
| 1   | 0   | 0   |
| 1   | 1   | 1    |

## Identities
- $p \rightarrow true \equiv true$
- $false \rightarrow q \equiv true$
- $p \rightarrow false = p$
- $true \rightarrow q = q$
