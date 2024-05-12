# Overview
This note covers how the optimal [[Query Processing|query processing strategy]] is selected

See first:
- [[Query Processing]]

# Selectivity
- A metric for evaluating how useful an index would be for a column
	- Indicates the uniqueness of values in the column
	- Higher = better
- Two types:
	- [[#Selection selectivity]]
	- [[#Join selectivity]]

## Selection selectivity
$$sl(A)$$ for an attribute $A$

- The ratio of tuples that meet a condition
	- I.E how likely a tuple is to match the condition

### Selection cardinality
$$s = r \cdot sl(A) \in [0, r]$$
where $r$ is the number of tuples

- The expected (average) number of tuples that will be matched by the condition

## Join selectivity
$$js = \frac{|R_{1} \bowtie R_{2}|}{|R_{1} \times R_{2}Z}$$
- The ratio of matching tuples in Cartesian space
	- I.E the likelihood of a joined tuple matching a condition

> [!INFO] Bowtie Operator
> $\bowtie$ means "$R_1$ join $R_2$"

### Join cardinality
$$jc = js\cdot |R_{1}| \cdot |R_{2}|$$

### Join selectivity theorem
$$
\begin{align*}
\text{Let: }&\\
n &= NDV(A, R_{1}) & A\mbox{ is an attribute of }R_1\\
m &= NDV(B, R_{2}) & B\mbox{ is an attribute of }R_2\\
\\
js &= \frac{1}{\max(n,m)}\\
jc &= \frac{|R_{1}| \cdot |R_{2}|}{\max(n,m)}
\end{align*}
$$
- Allows you to calculate the join selectivity and cardinality without knowing the join's result

# NDV
AKA the Number of Distinct Values

- The number of unique values in an attribute

# Strategies
- Query optimisation takes in a query and outputs an optimal [[Query Processing|execution plan]]

## Heuristic optimisation
- Built on top of [[Relational Algebra]]

## Cost-based otpimisation
- Generate multiple execution plans, estimate their [[Database Files#I/O access cost|cost]], pick the best one
- Uses a [[Database Files#Cost function|cost function]] with multiple parameters such as:
	- Number of [[Database Files#I/O access cost|block accesses]]
	- Memory required
	- CPU cost
	- Network cost

### Parameters
Takes the following information into account:
- The relations
	- The number of records $r$
	- The size of each record $R$
	- The number of [[Storage Blocks#Blocks|blocks]] $b$
	- The [[Storage Blocks#Blocking factor|blocking factor]]
	- How the [[Database Files|primary file is organised]]
	- The [[Indexing|indexes]] available
- Each attribute $A$ of each relation
	- The number of distinct values (NDV) $n$
	- The domain range ($[\min(A), \max(A)]$)
	- The type (continuous or discrete; [[Keys|key]] or non-key)
	- The level $t$ in the [[Indexing|index]]
- Each attribute $A$
	- The [[Probability Distribution#Probability functions|probability density function]] for each value $x$
		- A good approximation of the probability distribution is required

# Selectivity prediction
- Predicting the [[#Selection selectivity]] is usually intractable
- There are a few ways to get around this

## Approximation
- Use a histogram to approximate the [[Probability Distribution#Probability density function|PDF]]
	- Assumption: $sl(A = x) \approx P(A = x), x \in [\min(A), \max(A)]$
	- Just read off the value for the given $x$ and calculate the [[#Selection cardinality]]
- Accurate but adds maintenance overhead
	- You have to maintain a histogram --> you'd need to regenerate it every time the data in $A$ changes

## Uniformity assumption
- Assume the values are uniformly distributed $\therefore$ all values are equally likely
	- The probability of this is almost 0
- Less accurate than a histogram, but doesn't require maintaining a histogram
- $\forall x \in [\min(A), \max(A)] . sl(A = x) \approx k$
- [[#Selection selectivity]] $= \frac{1}{NDV(A)}= \frac{1}{n}$
	- [[#Selection cardinality]] $= r \cdot \frac{1}{NDV(A)} = \frac{r}{n}$

> [!EXAMPLE] Example: $A$ is a key
> Given an equality condition like `WHERE A = 1` for a *key* $A$, $sl(A = x) = \frac{1}{r}$ is a good estimate
> - [[#Selection cardinality]] will be 1 tuple (because all values are unique)

> [!EXAMPLE] $A$ is not a key
> Given an equality condition for a *non-key* $A$, $sl(A = x) = \frac{1}{NDV(A)} = 1/n$ is not a good estimate
> - If there are less distinct values than values ($n \ne r$), this will be a poor model
> 	- There are more tuples than values --> some values will have more tuples than others (unless $r$ is a multiple of $n$)
> - [[#Selection cardinality]] will be $\ge 1$ tuple
> 	- Because $n = NDV(A) \lt r \therefore \frac{r}{n} \ge 1$

## Cases
Here are the are the different cases for each type of query
### Range selectivity
Example query: `SELECT * FROM Relation WHERE A >= x`

- Domain range: $\max(A) - \min(A)$
- Query range: $\max(A) - x$
- The selectivity will be $0$ if $x > \max(A)$ (I.E $x$ is out of the domain  range)
- Otherwise, $sl(A \ge x) = \frac{\max(A) - x}{\max(A) - \min(A)} \in [0, 1]$

> [!WARNING] Assumptions
> Uniformity

### Conjunctive selectivity
Example query: `SELECT * FROM Relation WHERE A = x AND B = y`

- $sl(Q) = sl(A = x) \cdot sl(B = y) \in [0, 1]$
	- Applying the [[Joint Probability]] formula

> [!WARNING] Assumptions
> - Uniformity
> - $A$ and $B$ are [[Independence|indepedent]]

### Disjunctive selectivity
Example query: `SELECT * FROM Relationm WHERE A = x OR B = y`

- $sl(Q) = \left(sl(A) + sl(B)\right) - (sl(A) \cdot sl(B))$
	- Applying the [[Probabilistic Models#Sum rule|sum rule]]

> [!WARNING] Assumptions
> - Uniformity
> - $A$ and $B$ are [[Independence|indepedent]]

# Predicting cost
- The predicted number of blocks outputted will be $ceil\left(\frac{s}{f}\right)= ceil\left(\frac{r}{f \cdot NDV(A)}\right)= \frac{r}{nf}$ where:
	- The selection cardinality is $s = r \cdot \frac{1}{NDV(A)} = \frac{r}{n}$ ([[#Uniformity assumption]])
	- $f$ is the [[Storage Blocks#Blocking factor|blocking factor]]

> [!INFO] Key conclusion
> The expected number of blocks outputted reduces as blocking factor increases

- Accuracy can be improved by modelling cost as a function of $s(A) = \frac{r}{n}$ or $s(A) = P(A)$