# Definition
A number is prime if:
- $p > 1$
- The only _positive_ factors are 1 and itself, $p$

A number that fails this condition, it is called a _composite number_

# Fundamental theorem of arithmetic
Every positive integer > 1 is the unique product of some prime numbers
$$n = \prod\limits_{i=1}^{k}p_i^{e_{i}}= p_{1} \cdots p_{1}\times p_{2} \cdots p_{2} \cdots p_{k}\cdots p_k$$
Note that $\cdots$ represents many multiplications via $\times$

See also: [[Summation#Products|Product notation]]

# Coprime
- Two numbers are coprime if they share no common positive factors apart from 1
	- $GCD(x, y) = 1$

See also: [[GCD]]

# Euler's totient
$\phi(n)$
- A [[Functions|function]] that gets the [[Maths Map#Set theory|set]] of [[#coprimes]] less than or equal to the number

$$\phi(n) = \left| \{ x \in \mathbb{N} : 1 \leq x \leq n \land \text{gcd}(x, n) = 1 \} \right|
$$

See also:
- [[Set Operations#Set builder|Set builder notation]]
- [[Set Operations#Cardinality|Set cardinality]]
- [[GCD]]