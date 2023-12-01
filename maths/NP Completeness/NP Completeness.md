# Overview
NP completeness is an idea used to prove that algorithms are intractable (impossible to compute in a reasonable amount of time)

# Motivation
- Some problems have no polynomial-time algorithm
- It may be difficult to prove this
- Instead of proving that it's intractable, you can prove that it's as difficult as other problems that are likely intractable
- This creates a class of problems $NP$ that are all likely intractable

# Formal definition
A [[#Decision problems|decision problem]] is NP-complete if:
1. $\Pi \in NP$
2. $\forall \Pi' \in NP. \Pi' \propto \Pi$
	- There is a [[#Polynomial time reduction|PTR]] from all other problems in $NP$ to the problem
	- I.E it is at least as hard as all other problems in $NP$

## Theoretical consequences
The big question is whether the existence of an [[NDAs|NDA]] implies that a problem is solvable in polynomial time

Either:
- If $\Pi$ is NP-complete and $\Pi \in P$, $P = NP$
- If $P \neq NP$, if $\Pi$ is NP-complete, $\Pi$ cannot be in $P$

Finding an answer to this question would have a huge impact on the field of [[CS Map|Computing Science]]

# Definitions
- Intractable: a problem that cannot be solved in [[Time Complexity#Example growth rates|polynomial time]] (I.E it is $O(c^{n})$ or, worse, $O(n!)$)
- $P$: the class containing all problems that can be solved in polynomial time
- $NP$: the class containing all problems that can be solved in [[NDAs|non-deterministic polynomial time]]
	- $NP$ is probably a super-set of $P$ (yet to be proven)
- NP complete: the problem can be solved in non-polynomial time and it is at least as hard as all other problems in $NP$
	- See [[#Formal definition]]
- NDAs: a non-deterministically solvable algorithm
	- See [[NDAs]]
- $\Pi$: a problem
- $I$: an instance

# Decision problems
- When talking about NP-complete problems, we usually only consider decision problems (problems that return "yes" or "no")
- Searching and optimisation problems typically have a related decision problem
	- If the decision problem is in $P$, the search/optimisation problem likely is too

## Instances
An instance $I$ of a problem $\Pi$ can be either a "yes" instance or a "no" instance

# Polynomial time reduction
A function $f$ that converts one decision problem $\Pi_1$ to another $\Pi_{2}$ where
1. $f(I_1)$ constructs an instance of $\Pi_2$ in polynomial time
2. "Yes" instances remain "yes" instances

$\Pi_{1} \propto \Pi_2$ means there is a PTR from $\Pi_1$ to $\Pi_2$

## Formal definition
- There is a function $f: \Pi_1 \rightarrow \Pi_2$ where for each instance $I_1$ of $\Pi_1$:
	- An instance of $\Pi_2$ can be constructed via $f$ in polynomial time
		- $f(I_1)$ is an instance of $\Pi_2$
	- $f(I_1)$ is a "yes" instance in $\Pi_2$ if it is a "yes" instance in $\Pi_1$

## Properties
- Transitive: $\Pi_{1}\propto \Pi_{2} \land \Pi_{2} \propto \Pi_{3} \rightarrow \Pi_{1} \propto \Pi_3$
	- If there is a PTR from $\Pi_1$ to $\Pi_2$ and a PTR from $\Pi_2$  to $\Pi_3$, there must be a PTR from $\Pi_1$ to $\Pi_3$
		- The function will be $g(f(I_1))$
- Retains class: $\Pi_{1} \propto \Pi_{2} \land  \Pi_{2} \in P \rightarrow \Pi_{1} \in P$
	- If $\Pi_2$ is in $P$ and there is a PTR from $\Pi_1$ to $\Pi_2$, $\Pi_1$ is also in $P$
	- Meaning if $\Pi_2$ is solvable, $\Pi_1$ is solvable without much more effort
		- $\Pi_2$ could be harder to solve, but we only care about the easier instances of $\Pi_2$

## Use case
- If you can reduce one problem to another, easier, problem and you can solve the easier problem, the original problem is also solvable
- This builds a chain of problems
- If it is proven that a problem somewhere in the chain is in $P$, the whole chain must be in $P$
	- This means that problems that are very difficult to prove whether they are in $P$ could be proven by the existence of a much easier problem in $P$

# Proof
- If you can prove there is a [[#Polynomial time reduction|PTR]] from a known NP-complete problem to the problem, that is enough to know that the problem is at least as hard as all other NP-complete problems
- You don't need to prove this for each NP-complete problem, just one
	- This is because it propagates via the [[#Properties|transitive property]]
- The problems don't need to be similar for there to be a PTR

## Process
1. Prove that $\Pi_{1}\in NP$ 
	- Create an [[NDAs|NDA]] for the problem
2. Show that there is a [[#Polynomial time reduction|PTR]] from an NP-complete problem $\Pi_2$ to $\Pi_1$
3. Use the reduction function on an instance
	- Show that the construction can be done in polynomial time
	- Prove that "yes" instances are converted to "yes" instances
		- Assume $I_2$, an instance of $\Pi_2$), is a "yes" instance and show that $f(I_2)$ is also a "yes" instance
		- Assume that $f(I_2)$ is a "yes" instance and show that $I_2$ is a "yes" instance
		- I.E prove both sides of the assumption that "yes" instances are converted correctly


# Restrictions
- A restriction of a problem is a subset of the instances of the original problem
- If the restriction is NP-complete, $\Pi$ must also be NP-complete
- If $\Pi$ is NP-complete, the restriction isn't necessarily NP-complete

# Workarounds
If you have an NP-complete problem, you can work around it in order to increase the number of solvable instances by:
- Solving a restricted version of the problem
- Find an algorithm that is better, but still intractable
	- E.G an exponential-time algorithm that is better than exhaustive search
- For optimisation problems: approximate
	- Ideally, your algorithm should be provably good (evaluated using some heuristic)
- For decision problems: make a probabilistic algorithm that usually gives the correct answer 