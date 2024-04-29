# Overview
This note covers the complexities of dealing with continuous random variables

See first:
- [[Probability Distribution#Probability density function|Probability density functions]]

# Problems
- A PDF doesn't output the probability of $x$, but the density at $x$
	- The probability for any $x$ will be 0 because it's extremely unlikely that two randomly sampled real numbers will be equal
	- Instead, use a [[#Within range|range]]
- The density can be > 1
	- Use [[#Cumulative distribution functions|CDFs]] instead

# Within range
$$P(X \in (a,b)) = (a \lt X \lt b) = \int_{a}^{b} f_{X}(x)$$
- The probability of $X$ being in the range $a..b$

# Support
$$supp(x) = \{ x | f_{X}(x) \gt 0 \}$$
- A [[Functions|function]] that finds the regions where the density is 0

| Situation                                                                               | Name             |
| --------------------------------------------------------------------------------------- | ---------------- |
| There is support in just one region                                                     | Compact support  |
|                                                                                         |                  |
| There is support in the whole function                                                  | Infinite support |
| A portion of the function has infinite support (E.G $1..\infty$ but not $-\infty .. 1$) | Semi-infinite    |

# Cumulative distribution functions
AKA CDFs

$$F_{X (x)}= \int_{-\infty}^{x}f_{X (x)}= P(X \le x)$$
- Basically a [[Probability Distribution#Probability density function|PDF]] that can only map to $0..1$
- Outputs the probability mass less than or equal to $x$
- Used to get the probability for a range
	- E.G $P(3 \le X \le 4) = F_{X} (4) - F_{X} (3)$

