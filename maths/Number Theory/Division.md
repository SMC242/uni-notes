# Divides operator
$a | b$ means $a$ divides $b$ cleanly (there is no remainder)

Implications:
- $a \neq 0$
- $\exists c. (a \times c = b)$
	- $b$ can be calculated by multiplying $a$ by some factor, $c$
https://moodle.gla.ac.uk/pluginfile.php/5699780/mod_resource/content/3/af2_week4.pdf

# Algorithm
Division can be split into multiple variables

- Dividend: $a \in \mathbb{Z}$
- Divisor: $d \in \mathbb{Z}^+$ 
- Quotient: $q \in \mathbb{Z}$ and unique
- Remainder: $r \in \mathbb{Z}^+$ and unique
	- $r \lt d$

Formula:
$$a = d \times q + r$$