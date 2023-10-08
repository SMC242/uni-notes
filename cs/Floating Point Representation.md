# Overview
- Floats are a way to represent real numbers
	- Deterministic approximations
	- Used for huge, tiny, or fractional numbers
- They are used for scientific, statistical, and graphical computations
	- **NEVER** use them for financial programming
	- See: [[#Financial Decimals]]
- There are two common sizes of floating point numbers: 32 bit (float) and 64 bit (double \[precision\])

See also: [The standard for floating point representation](https://en.wikipedia.org/wiki/IEEE_754)

# Format
- A sign bit
- [[#Mantissa]]
- [[#Exponent]]
![Example float](https://courses.physics.illinois.edu/cs357/sp2020/assets/img/figs/ieee_single.png)

- Similar to scientific notation: $\textrm{mantissa} \times 10^{\textrm{exponent}}$

## Mantissa
- The size of the mantissa defines the range that can be represented
- Represents a number in the range: $1.0 \le m \lt 2.0$
- There is always one and only one digit before the decimal section
	- Just like scientific notation
	- This is implicitly assumed to be 1 and is not stored
- Always positive

## Exponent
- The shift applied to the [[#mantissa]]
- There is an implied offset to allow for negative numbers
	- This is set at the type level
	- `float32`s use `offset = -127`
- 

## Example
$$
\begin{align}
\textrm{To represent 15,654:} \\
& m := 1.5654 \\
& exp := 4  \\
& m \times 10^{exp} = 15,654
\end{align}
$$

# Intricacies 
- The representable numbers are more dense near 0, and less dense far from 0

## Exceptions
The following errors can occur when doing floating point operations:
- Invalid operation: impossible operations like $\sqrt{-1.0}$ and $\frac{0.0}{0.0}$
- Division by zero
- Overflow: the result is larger than maximum representable number 
- Underflow: the result is smaller than the minimum representable number
- Inexact: rounding has occurred

### Trapping
- You can decide whether you want the OS to throw an error if any of these exceptions occur
- If you decide you want this, the exception is said to be "trapped"
- This can decided on a per-operation and per-exception basis
- If untrapped, some default value will be outputted

| Exception         | Trapped by default? |
|-------------------|---------------------|
| Invalid operation | Yes                 |
| Division by zero  | Maybe               |
| Overflow          | Maybe               |
| Underflow:        | No                  |

## Round-off error
- Since the precision is limited, numbers that fall between the cracks are rounded to the nearest representable number
- Some numbers can't be represented in decimal form (E.G rational numbers like $\frac{1}{3}$)
- This can introduce a significant amount of error after successive floating-point operations

### Dangerous operations
- $x + y \textrm{ where } |x| \ne |y|$ (magnitude error)
	- Adding small numbers to big numbers
- $x - y \textrm{ where } x \approx y$ (cancellation error)
	- Subtracting numbers that are almost equal
- Comparing floats
	- Due to the imprecision of floats, two numbers that would logically be equal may not be equal in their representation

### Round-off error mitigation
- Compare using an error tolerance $\epsilon$
	- Use $|x - y| < \epsilon$ ([libraries will provide a function for this](https://numpy.org/doc/stable/reference/generated/numpy.allclose.html))
	- This is usually defined by the library you are using
- Machine epsilon
	- The margin of error defined by IEE754 and implemented by the hardware
	- Gives you confidence about the error of floating point operations and their storage
- 

# Financial decimals
- Due to [[#Round-off error]], floats are not appropriate for financial computing
- Languages have specific types that can be used for this purpose
	- [C#](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/floating-point-numeric-types#characteristics-of-the-floating-point-types)
	- [Java](https://docs.oracle.com/javase/8/docs/api/java/math/BigDecimal.html)
	- [Python](https://docs.python.org/3/library/decimal.html#decimal.Decimal)
# Special numbers
## Zero
- `+0.0` and `-0.0` are different at the bit level (due to the sign bit)
- At the software level, they are equal

## Infinity
- $+\infty$ and $- \infty$ can be encoded
- The exponent is all `1`s and the mantissa is all `0`s

## NaN
- Not a Number
	- Represents results that do not exist
		- $\frac{0}{0}$
		- $\frac{\infty}{\infty}$
		- $\infty - \infty$ and $\infty + \infty$
		- $\infty \times 0$
		- $\sqrt{x} \textrm{ where } x \lt 0$  
		- $\log{x} \textrm{ where } x \lt 0$
	- Numbers that are not there
		- Placeholders in datasets
- `NaN != NaN` and not equal to anything else
- Propagates through operations, causing every subsequent operation to output `NaN`
- Represented as a float where the exponent is all `1`s and the mantissa is not 0