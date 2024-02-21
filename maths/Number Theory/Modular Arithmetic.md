# Overview
Built on top of the [[Modulo]] operator and the foundation for [[Asymmetric Cryptography]]

> [!NOTE] Reducing
> In the context of modular arithmetic, "reducing" an integer modulo $n$ means applying some $f: (x, y) -> z$  to the left operand of a $mod$
>
> Example: $(x \times y) \mod 3$
> See also: [[Functions]]

# Remainders
- After reducing a number modulo $n$, $0, 1, \dots , n - 1$ appear periodically
	- $\forall x \forall n \in \mathbb{Z}^{+}. x \mod n \in \{0, 1, \dots , n - 1\}$

# Rules
- $(x + y) \mod n = (x \mod n + y \mod n) \mod n$
- $(x - y) \mod n = (x \mod n - y \mod n) \mod n$
- $(x \times y) \mod n = (x \mod n \times y \mod n) \mod n$

# Efficient exponentiation
- $x^{e} \mod n$ quickly becomes unfeasible to compute as $e$ increases
- Instead, use the binary representation of $e$:
	- $e = \sum\limits_{i=0}^{k-1} e_{i}\cdot 2^{i}$
- Then apply the [[#Rules|modular multiplication rule]]
	- $x^{2} \mod n = (x \times x) \mod n = (x \mod n \times x \mod n) \mod n$
	- $x^{4} \mod n = (x^2 \times x^2) \mod n = (x^2 \mod n \times x^2 \mod n) \mod n$

> [!NOTE] General Form of Reductions Over Modular Exponentiations
>$$
 \begin{align*}
 x^{e} \mod n &= x^{\sum\limits_{i=0}^{k-1} e_{i} \cdot 2^{i}} \mod n\\
 &= (\prod\limits_{i=0}^{k-1} x^{e_{i} \cdot e^{i}} \mod n)\\
 &= (\prod_{i:e_{i}=1} x^{2^{i}} \mod n) \mod n
\end{align*}
> $$
>
> N.B: the colon notation means "$i$ while $e_{i} = 1$"

# Multiplicative inverses
- Building on the [[GCD]] operator
- $x \cdot  A + n \cdot B = \gcd(x,n)$
	- $A$ and $B$ can be computed using the [[GCD#Euclidean algorithm#Extended|Extended Euclidean Algorithm]]
- $A$ is the multiplicative inverse of $x \mod n$
	- $(x \cdot A) \mod n = 1$
