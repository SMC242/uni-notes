---
tags: Protocol 
---
# Overview
An old protocol for exchanging [[Asymmetric Cryptography]] keys

See first: [[Cyclic Groups]]

# Cyclic groups
- [[Cyclic Groups]] are used in this protocol

> [!EXAMPLE]
> - $G = \{a^{2i} \mod p | i = 0, \dots, q - 1\}$ where:
> 	- $p, q$ are [[Primes|prime numbers]] such that $p = 2q + 1$
> 	- $a$ is an integer such that $\{1, 2, \dots, p - 1\} = \{a \mod p, a^{2} \mod p, \dots, a^{p-1} \mod p \}$
> - The generator is $g := a^{2} \mod p$ [[Cyclic Groups#Notation|under]] multiplication [[modulo]] $p$
> 	- $g^{x} \times a^{2} \mod p$

# Protocol
1. Use a publicly known group $G$ [[Group#Order|of order]] *some prime number $q$*
	- $g$ is the generator of $G = \{g^{0}, g, g^{2}, \dots, g^{q-1}\}$
2. Party 1 does:
	1. Pick a random integer $x$ from $\{0, \dots, q - 1\}$
	2. Compute $h_{A} \leftarrow g^{x}$
	3. Send $h_{A}$ to party 2 via a public channel
3. Party 2 does the same, picking a $y$ and sending an $h_{B}$ to party 1
4. Both parties compute keys based on the $h$s they received
	1. $k_{A} \leftarrow (h_{B})^x$
	2. $k_{B} \leftarrow (h_{A})^x$
5. They now have the same key

## Proof
$$
\begin{align*}
k_{A} &= (h_{B})^{x}\\
&= (g^{y})^{x}\\
&= g^{y \cdot x \mod q}\\
&= g^{x \cdot y \mod q} & \mbox{apply commutative property}\\
&= (g^{x})^y\\
&= (h_{A})^y\\
&= k_{B}
\end{align*}
$$

# Security
- A [[Adversaries#Passive|passive adversary]] (eavesdropper) can see both $g^{x}$ and $g^y$
- If $q$ is large enough, it's not feasible to compute:
	- $a$ given $g^{a}$
		- AKA discrete logarithm problem
	- Whether $g^{x \cdot y \mod q}$ is not a random element from $G$ based on $g^{x}$ and $g^{y}$
		- AKA decisional Diffie-Hellman problem
- Can't stop man-in-the-middle attacks because there is no [[authentication]] or [[cryptographic signing]]
	- Someone can modify the messages in transit
- 