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
- The ratio of matching tuples in Cartesian space
	- I.E the likelihood of a joined tuple matching a condition

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
- Less accurate than a histogram, but doesn't require maintaining a histogram
- $\forall x \in [\min(A), \max(A)] . sl(A = x) \approx k$
- 