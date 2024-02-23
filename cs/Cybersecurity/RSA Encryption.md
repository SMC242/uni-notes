# Overview
A [[Public-key Encryption]] scheme based on the following idea:

$$\begin{align*}
Let:\\
p, q &= \text{ 2 distinct primes}\\
n &= pq \\
\phi(n) &= (p - 1)(q - 1)\\
f_{e}(x) &= x^{e} \mod n & \mbox{e > 0}\\
M &\in \{0, 1, \dots, n -1 \}\\\\
\end{align*}
$$
$$
\begin{align*}
& \text{If } \gcd(e, \phi(n))= 1 \text{ then}\\
& - f_{e} \text{ is a permutation}\\
& - \text{If } d = (e \mod \phi(n))^{-1} \times (e \mod \phi(n)) \text{ then } f_{d} = f_{e}^{-1}
\end{align*}
$$

> [!NOTE] Multiplicative inverse
> - The multiplicative inverse of a number is $x \times x^{-1}$. $(e \mod \phi(n))^{-1} \times (e \mod \phi(n))$  means the multiplicative inverse of $(e \mod \phi(n))$

See first:
- [[Modulo]]
- [[GCD]]
- [[Primes|Prime numbers]]

# Key generation
For input $1^k$
1. Randomly pick two distinct $k$-bit primes $p$ and $q$
2. Compute $n = pq$
3. Compute $\phi(n) = (p - 1)(q - 1)$
4. Pick an $e$ such that $1 \lt e \lt \phi(n) \land gcd(e, \phi(n) )=1$
5. Compute $d = (e \mod \phi(n))^{-1} \times (e \mod \phi(n))$
6. $sk := (d,n)$
7. $pk := (e, n)$
8. Return $(sk, pk)$

# Encryption
1. Take in a public key $(e,n)$ and a message $M$ (encoded as an integer $0 \le M \lt n$)
2. Compute $C \leftarrow M^{e} \mod n$

# Decryption
1. Take in a private key $(d, n)$ and a cipher text $C$ (encoded as an integer $0 \le C \lt n$)
2. Compute $M \leftarrow C^{d} \mod n$

> [!NOTE] How it works
> $f_{d}(x) = x^{d}\mod n$ is the inverse of $f_{e}(x) = x^{e} \mod n$
> $$
> \begin{align*}
Dec_{sk}(Enc_{pk}(M)) &= (m^{E} \mod n)^{d} \mod n\\
&= f_{d}(f_{e}(M))\\
&= M
\end{align*}
> $$

# Security
- Security depends on the difficulty of factoring $\mod n$ into $p, q$
	- If the attacker knows $p, q$, it can compute $\phi(n)$ and find the private key $d$
	- Large keys are considered safe (2048 bits) but won't be for long
- Not [[IND-CPA]] secure because RSA is deterministic
	- Repeated messages using the same public key will output the same ciphertext
	- It is fine if the messages are random and unpredictable

# Variants
## Padded RSA
- Pad the message with random garbage
- If $\frac{random\ string\ length}{message\ length}$ is large enough, this is [[IND-CPA]] secure
	- I.E the padded message is mostly garbage
- Vulnerable if the attacker has access to the decryption algorithm
	- They can choose ciphertexts until they figure out the key ([[Chosen Ciphertext Attack]])

## Optimal Asymmetric Encryption Padding
- Not vulnerable against [[Chosen Ciphertext Attack]]s
- 