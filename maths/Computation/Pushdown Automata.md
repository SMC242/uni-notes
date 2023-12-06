# Overview
Pushdown automata extend [[Finite-state Automata]] by adding a stack for memory. They have these properties:
- Simple
- Unlimited memory
- Behaves like a stack

# Definition
A PDA has:
- An input alphabet $\Sigma$
	- A set of stack symbols $G$ (the alphabet of the stack)
	- $\epsilon$ is the empty symbol
- A finite set of states $Q$
	- There is one start state
	- There may be many acceptance states
		- A input is rejected if there is no transition defined for it at that particular instruction
- A transition relation (program)
	- $T \subseteq (Q \times \Sigma \cup \{\epsilon\} \times G \cup \{\epsilon\}) \times (Q \times G \cup \{\epsilon \})$
	- E.G $(q_{0}, a, b) \rightarrow (q_{1}, c)$

# Transition relations
A program has 2 components:
- $(q, \sigma, g)$ where $q \in Q, \sigma \in \Sigma, g \in G$
	- If in state $q$, read $\sigma$, add $g$ to the stack if the read was successful
- $(q, g)$
	- If successful, transition to state $q$ and pop $g$ from the top of the stack

> [!NOTE] No-ops
> $\epsilon$ can be passed to a $\sigma$ or $g$ argument when you don't want to read or pop from the stack
> 
> Examples:
> - $(q_{1}, \epsilon, \epsilon) \rightarrow (q_{2}, \epsilon)$ will simply transition from $q_1$ to $q_2$
> - $(q_{1}, \epsilon, h) \rightarrow (q_{1}, \epsilon)$ won't read or pop but will add "h" to the stack

> [!EXAMPLE] Example program
> ![Example](https://cs.uwaterloo.ca/~eblais/cs365/docs/grammars/PDA.png)
>
>This program reads a binary sequence until an empty symbol has been reached 

## Checking for empty stack
- There is no specific instruction for this
- It is conventional to add a `$` to the stack at the start of the program
- This allows you to check for an empty stack by attempting to pop `$`

# Non-deterministic pushdown automata
- Unlike [[Finite-state Automata]], non-deterministic PDAs can do more things than deterministic PDAs
- Being non-deterministic allows them to guess
	- This allows them to work around being unable to read backwards
		- Useful in situations where future information matters (E.G where the middle of the input is)

## Palindromes
A problem where non-determinism is required is the palindromes problem

> A palindrome is a sequence of characters that reads the same forwards and backwards (I.E the second half is the reverse of the first)

Since PDAs can only do one pass through the input, they have to guess where the middle of the input is