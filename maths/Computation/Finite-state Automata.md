# Overview
A [[Computational Models]] with these characteristics:
- Fixed amount of memory
- Simple
- Limited problem-solving ability

They are the basis for [[#Regular languages|RegEx]]

# Deterministic finite-state automata
- Recognises a finite input alphabet $\Sigma$  from a read-only tape
	- This defines the language accepted by the DFA
- A finite set of states $Q$
- An initial state $q_0$ and a set of accepted states $F \subseteq Q$
- A program known as a transition relation $T \subseteq (Q \times \Sigma) \times Q$
- For any state and action, there is only one transition defined

> [!EXAMPLE] Example program
> `((q,a), q')` translates to "if in state `q`, read `a` and move to state `q'`"

## DFA diagrams
- An incoming arrow is the initial state
- Nodes with double borders are acceptance nodes
	- If this node is reached, the input is accepted

> [!EXAMPLE]
> ![DFA diagram](https://media.geeksforgeeks.org/wp-content/uploads/20201027182043/1.png)
>
> This program accepts:
> 1. One or more "b"s
> 2. One or more "a"s
> 3. If a "b" is encountered, the previous pattern should repeated
> 4. Otherwise, there should be an "a"
> 5. If this pattern was satisfied, the input will be accepted

# Non-deterministic finite-state automata
- Like DFAs, except they can make choices
	- For each state and input symbol, there can be 0 or more possible transitions
- All NFAs can be converted to DFAs

> [!EXAMPLE]
> ![Example](https://media.geeksforgeeks.org/wp-content/uploads/00nfa.png)

See also: [[Non-deterministic Algorithms]]

## NFA reduction
- NFAs can be reduced to DFAs
- This can make the number of states balloon from $n$ to $2^n$ in the worst case
	- This is called a "blow-up" in the number of states

This is the process:
1. For each state in the NFA, create a subset of states in the DFA. This subset contains all possible states the NFA could transition to after reading a particular symbol
2. Create a transition function for the new DFA
3. The initial state is the set of initial states of the NFA
4. The final state is the set of states in the NFA that has at least one final state
5. Show that the resulting DFA accepts the same language as the original DFA

> [!EXAMPLE]
> Original NFA:
> ![NFA](https://static.javatpoint.com/tutorial/automata/images/automata-conversion-from-nfa-to-dfa.png)
>
> Resulting DFA:
> ![DFA](https://static.javatpoint.com/tutorial/automata/images/automata-conversion-from-nfa-to-dfa2.png)

# Regular languages
- A language that can be specified with a finite-state automaton
- A regular language can be specified by a regular expression over an alphabet $\Sigma$
	- $\epsilon$ is the empty string (which can be a regular expression)
	- $\sigma$ is a regular expression containing symbols from $\Sigma$

> [!EXAMPLE] Example regular expression
> $R = (ac | a*b)d$ means:
> 1. The input must start with "ac" or 0 or more "a"s and a "b"
> 2. The next and final character will be a "d"

## Operations
The following operations are defined for regular expressions:

Let $R, S$  be regular expressions
- Concatenation: $RS$
- Choice: $R | S$ (tests $R$ then $S$ if the input is not accepted)
- 0 or more copies of an expression: $R*$
	- Known as a "closure"
	- Implemented as a union of an infinite series of concatenations of $R$
		- I.E $for R*, L=a: \{a \cup aa \cup aaa \cup aaaa \cup ...\}$
		- Other implementations require too much memory
- Precedence override: $(R)$
- Complement (negation): $\lnot x$
	- Equivalent to all characters in $\Sigma$ apart from $x$
- Any character: $?$
	- Equivalent to all characters in $\Sigma$

### Precedence
This is the order of precedence:
1. Closures
2. Concatenations
3. Choices

Brackets can be used as an escape hatch

# Limitations
- Can't handle $a^{n}b^{n}$ (I.E there are an arbitrary but equal number of "a"s and "b"s)
	- This would require memory

Solution: use [[Pushdown Automata]] instead