# Overview
The Halting Problem is an [[Computability#Undecidable problems|undecidable problem]]

> Instance: a program $X$ and an input string $S$
> Question: does $X$ halt when run with $S$

# Proof
[[Proofs#Contradiction|Proof by contradiction]]

- Assume that you have a program $Q(W)$ that solve the Halting Problem
- Create a program $P(W)$ that runs $Q$ and:
	- If $Q(W) = yes$, infinitely loops
	- If $Q(w) = no$, halts
- Now pass $P$ to itself $P(P)$
- This will never halt
	- The program won't halt if $Q$ returns "yes" because it gets caught in an infinite loop
	- The program must therefore return "no"
		- But $P$ would terminate if "no" was returned
