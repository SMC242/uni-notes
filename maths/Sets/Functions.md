# Overview
- A function maps each element of a domain to an element of a co-domain
- They are considered to be valid if they are [[#Bijective]]

# Notation
Functions are defined in the form $f: X \rightarrow Y$
	where
		$X$ and $Y$ are sets
		$f$ is some function
		
Notice how similar this is to type annotations in programming languages

# Mapping
- A function from $X$ to $Y$ is actually a set of tuples containing all mappings -> $f = X \times Y$
	- It's like getting the cross-product
Such a set might look like $$f=\{(2, 1), (4, 2), (6, 2), (8, 4), ...\}$$
That was the set for $f: \mathbb{N}_e \rightarrow \mathbb{N}_e$ where $f(x) = \frac{x}{2}$

## Images
- An image is the mapping for a given value
- A preimage is the value passed into the function to get the image
$y$ is the image in $f(x) = y$ and $x$ is the preimage

## Range
The range of a function is the set of all possible images of $X$

# Composition
- Two functions can be joined together
	- This might be done with addition, multiplication, or simply calling $f(x)$ on the output of $g(x)$
- $f$ must be a subset of the domain of $g$
	- I.E the output type of $g(x)$ must be compatible with the input type of $f(x)$

# Injective functions
- A function where the all values of $X$ have a mapping, but not all values of $Y$ have a mapping
- The domain of $X$ is smaller than $Y$
	- $|X| \le |Y|$

## Strictly increasing/decreasing functions
- Always injective
- $f(x_1) < f(x_2)$

# Surjective functions
- $X$ each value of $X$ may have multiple mappings
- The domain of $Y$ is smaller than $X$
	- $|Y| \le |X|$

![Fuction Diagram](https://qph.cf2.quoracdn.net/main-qimg-661500bb793c5d5166b454d0a0eba35f-lq)

# Bijective functions
- Both injective and surjective
- Meaning that all values of $X$ map to at least one value of $Y$

## Inverse functions
- Function required to be bijective
- The opposite of a function
- Example: $f(x) = 2n$, $f^{-1}(x) = \frac{n}{2}$