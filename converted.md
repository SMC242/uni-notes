---
aliases:
  - MCMC
---

# Overview
> [!DANGER] Challenges
> Computing [[Bayes' Theorem]] is difficult because:
> - $P(D | \theta)$ needs to be a distribution function instead of just a value
> - $P(D) = \int_{\theta} P(D | \theta) \cdot P(\theta)$ is often infeasible

- Built on top of [[Sampling#Monte Carlo|Monte Carlo]]
- Allows you to [[Sampling|sample]] from distributions that you can't sample from directly
	- Such as when you're doing [[Statistical Inference#Bayesian inference|Bayesian inference]]


> [!WARNING] Drawback
> Sampling strategy has a big influence on the result of MCMC

See first:
- [[Bayes' Theorem]]

See also:
- [[Statistical Inference#Bayesian inference|Bayesian inference]]

# Trace
- All of the accepted samples are stored
- Model parameters are estimated using the histogram of this trace
- Notation: $[x^{(1)}, x^{(2)}, \dots, x^{(n)}]$# Bar plots
These are usually used for analysing a categorical variable.

## Side-by-side
The most common
![[side_by_side.png]]

## Stacked
Used for showing the total value when a categorical variable is further broken down into another field.
	![[stacked.png]]

## Normalised stacked
Used for demonstrating the total *proportion* of a categorical variable that is broken down by another field.
![[normalised.png]]

# Mosaic plot
Use area to represent proportion
![[Mosaic plot.png]]

# Pie charts
Good as a simple representation, unsuitable for analysis due to poor axes
![[Pie.png]]# Histogram
Often used for visualising the distribution
![[Histogram.png]]

# QQ plot
Used for comparing the points of a dataset to linear fit. This helps to see if a [[stats/Regressions/Linear/Linear Regression|Linear Regression]] is normally distributed and is actually linear.
![[Pasted image 20220503173057.png]]# Overview

- Estimating the parameters of the global population based on some observed data (the samples)
- Assumes there is an underlying distribution

See first:

- [[Probability Distribution]]

# Initial model

- [[Linear Regression]] can be used as a starting point for inference
- Assumption: each variable is just another variable with some scaling and shifting
- Higher dimensional form: $y = Ax + b$
  - See [[Matrices]]
- $y$ is a random variable, $x$ is known, $m, c, \sigma$ are parameters
  - Observations are of the form $(x_{1}, y_{1})$
- You can't actually use them for inference because linear regression just gives you a line for the observed data, not the unseen data
  - You could assume that this model works for the unseen data, but you'll be wrong

## Modelling epsilon

- A noise term $\epsilon$ can be added to account for real noise
- You need to assume the distribution of this term
  - Assuming that $\epsilon$ is [[Normal Distribution|normally-distributed]]: $y = mx + c + \mathcal{N}(0, \sigma^2)$
  - Using $mx + c$ as the mean: $y \sim \mathcal{N}(mx + c, \sigma^2)$

# Methods

## Direct estimation

- Defining [[Functions]] of observations to estimate the parameters
  - Requires [[#Estimators|estimator functions]] for each distribution
- Efficient

> [!EXAMPLE] Examples
>
> - [[Mathematical Optimisation#Linear least squares|Least squares]]
> - [[Matrix Operations#Pseudo-inverse|Pseudo-inverse]]

### Estimators

- Functions that take in a set of observations and return the estimated parameters
- You have to assume the distribution (E.G that it is [[Normal Distribution|normally distributed]])
  - This is called the model
- Must be created for each problem

> [!EXAMPLE] Common Estimators
>
> - Arithmetic mean
> - Sample mean
> - Variance
> - Standard deviation

## Maximum likelihood

AKA log-likelihood optimisation, MLE

- Start with a model
- [[Mathematical Optimisation|Optimise]] [[Odds#Log-odds|log-likelihood]] by changing the parameters of the model
  - Likelihood: $\mathcal{L}(x_{1}, \dots, x_{n}) = \prod_{i} f_{X} (x_i)$
    - [[Independence]] required
  - Log-likelihood: $\log \mathcal{L}(x_{1}, \dots, x_{n}) = \sum\limits_{i} \log f_{X} (x_{i})$
  - The parameters are packed into $\theta$
    - These are what we're trying to optimise
- Works in situations where you know the likelihood function
- Used if you don't have an [[#Estimators|estimator]] for a parameter
- Notation for likelihood that depends on some parameters in a distribution $\theta$: $\mathcal{L}(\theta|x)$
- Objective function: $L(\theta) = - \log \mathcal{L}(\theta | x_{1}, \dots, x_{n} ) = - \sum\limits_{i} \log f_{X}(x_{i}; \theta)$
  - Aiming to minimise the negative log-likelihood avoids [[Floating Point Representation#Exceptions|underflow errors]]
  - [[Higher Order Optimisation#Gradient descent|Gradient descent]] can be used if this is differentiable

### Mixture models

- When there are multiple components in a distribution
  - Example: a distribution with two [[Normal Distribution|normal-like]] humps
  - Used to model clusters
- Use multiple models $\mathcal{N}_i(\mu_{i}, \sigma_{i})$ with weighting factors $\lambda_i$
  - The weighting factors must add up to 1
  - Indicates the importance of a component (I.E how likely an observation is to fall in that component)
- Problem: no estimators exist for these models
- Solution: just optimise
  - The likelihood function is $\mathcal{L}(\theta | x)$ and is the log of the weighted sum of the [[Probability Distribution#Probability density function|PDFs]]
  - The parameter vector made up of all the $\mu, \sigma, \lambda$s

## Bayesian inference

- Uses distributions over parameters instead of direct values
  - Outputs the _models that are compatible with the data_ rather than a single best-fit model
- Start with initial guesses for the parameters (the initial hypotheses or "prior"s)
- Consider the parameters as random variables
  - This means we can estimate the distribution of their values
- Update the parameters based on the [[Odds#Likelihood|likelihood]] of a parameter taking on a value
  - Observations are used to create a tighter distribution (the "posterior")
  - The distribution would change as more observations are added to the dataset
- Posterior formula: $P(\theta|D) = \frac{P(D | \theta) \cdot P(\theta)}{P(D)}$ where $D$ is the data, $P(\theta)$ is a prior over parameters
- The predictive posterior is the expected distribution of observations
  - If the model is good, sampling from this will be like sampling from the real distribution
  - I.E it approximates the real distribution

Requirements:

- A prior $P(\theta)$ over a parameter vector
  - I.E an [[#initial model]] including values for $m$, $c$, and $\epsilon$
- A likelihood function $P(D | \theta)$
- A way to combine them into the posterior formula
  - $P(D)$ is usually infeasible to compute
  - Instead, approximate using [[Sampling#Monte Carlo|Monte Carlo]]

See also:

- [[Bayes' Rule]]

> [!TIP]
> Bayesian inference is the process of updating your prior beliefs based on new evidence

### Models as graphs

AKA graphical models

- Expressions can be converted to graphs
- Used to model dependencies between random variables
  - Some variables will have been observed, others won'will be unobserved
  - Dependencies are either deterministic (defined by the parent) or stochastic (defined by a distribution)
    - Stochastic nodes require prior distributions
- You can do inference on the graph
  - You can find the posterior distribution of the unobserved nodes (I.E infer their distribution) by integrating over the possible values given the observed values (the evidence)
- Variables can be fixed (assigned a fixed value) if you can express this knowledge as a distribution
- Stochastic nodes that don't depend on a deterministic node are not allowed
  - Such nodes are known as "wild nodes"

> [!EXAMPLE] > $$y = mx + c$$
>
> ```mermaid
> graph RL
> 	y
> 	m
> 	x
> 	mx
> 	c
> 	m --> mx
> 	x --> mx
> 	mx --> y
> 	c --> y
> ```

> [!EXAMPLE] Detailed Example
>
> $$
> \begin{align*}\\
> y &\sim N(\mu, \sigma)\\
> \mu &= mx + c
> \end{align*}
> $$
>
> ```mermaid
> graph RL
> 	y(y)
> 	m(m)
> 	x[x]
> 	mx(mx)
> 	c(c)
>     μ(μ)
>     σ(σ)
>
> 	m -.-> mx
> 	x -.-> mx
> 	mx -.-> μ
> 	c -.-> μ
>     μ --> y
>     σ --> y
> ```
>
> - Dotted lines are deterministic dependencies
> - Solid lines are stochastic dependencies
> - Circular nodes are random variables
> - Square nodes are observed variables
> - Key assumption: there is some invisible structure that can be modelled as a linear relationship to $x$

### Computing

- Computed using [[Markov Chain Monte Carlo]]
- We assume that $P(\theta | D) = \frac{P(D | \theta) P(\theta)}{\int_{\theta} P(D | \theta) P(\theta)} \propto P(D | \theta) P(\theta)$
  - I.E that $P(D)$ is just a normalising constant
- Make assumptions about the distributions for your stochastic variables (E.G $m, c, \sigma$)
- Find a likelihood function
  - For a linear regression, it's $L(D | \theta) = L(x, y; m, c, \sigma) = f_{X} (y - mx + c, \sigma^2)$
- The histogram of posterior samples is called the "trace"
-
# Overview
Statistics is the field of summarising and understanding data

See also: [[Research Map|Research Methods]]

# Core theories
- [[Events And Outcomes]]
- [[Independence]]

# Probability theory
Finding out how likely an outcome is

- [[Disjoint Events]]: modelling events that can't happen together
- [[Non-disjoint Events]]: events that can happen together
- [[Probabilistic  Models]]: why it's useful to make predictions
- [[Probability Distribution]]: how to measure the probability of outcomes
- [[Joint Probability]]: the probability of two outcomes together
- [[Conditional Probability]]: the probability given prior information
- [[Predictions]]: expecting what a value will be
- [[Odds]]: how to talk about probability
- [[N-gram Models]]: modelling data as tuples

# Distributions
How data is distributed and properties of those distributions

- [[Normal Distribution]]: the most common distribution
	- [[Z Score]]: a metric of spread used when dealing with normal distributions
- [[Geometric Distribution]]: estimating how many trials are needed to guarantee an outcome
- [[Binomial Distribution]]: estimating the number of desired outcomes in a number of trials
- [[Poisson Distribution]]: estimating the number of desired outcomes in a *period of time*

See [[Which?]] for how to pick a distribution based on what you're estimating

# Sampling
How to take data from a distribution

- [[Sampling]]: the core theories behind sampling
	- [[Central Limit Theorem]]: why we can use the [[Normal Distribution]] so much
- [[Continuous Distributions]]: how to sample for continuous variables
- [[Multivariate Distributions]]: how to sample vectors

# Inference
Finding out information about the population from a sample

- [[Statistical Inference]]

# Hypothesis testing
Testing claims about data

- [[Hypothesis Testing]]
	- [[Confidence Interval]]: the range that you expect values to fall in
	- [[Conclusion Errors]]: the types of mistakes you can make while hypothesis testing
- [[T test]]: if the difference between two samples is noticeable

# Regressions
Modelling the tendency of data as a line

- [[Linear Regression]]
	- [[Least Squares Regression]]: a method for linear regression
	- [[Categorical Regressions]]: regressions on categorical variables
	- [[Hypothesis Testing on Linear Regressions]]
	- [[Outliers]]: how to handle unusual data
- [[Multiple Regressions]]: linear regression with multiple variables
	- [[Checking Conditions]]: some conditions are required. How do we check them?
	- [[Improving Models]]: how to make a model better
	- [[Pruning Models]]: removing unhelpful variables
- [[Residuals]]: a measure of error in a linear regression model
- [[Logistic Regression]]: regressions that output a binary answer

# Time series
Analysing data with a time dimension

- [[Seasonal Trends]]
- [[Trends]]: types of trends

# Overview
A fundamental operation that breaks a function into a sum of sinusoids (functions that use the $\sin(\theta))$ function. These waves sum to create the function

- The function must be repeating
- Analyses functions in terms of amplitude and phase over frequencies (rather than by amplitude over time)

> [!NOTE] Sine Wave Formula
> $$x(t) = A \sin (\omega 2 \pi t + \theta)$$
> where:
> - $A$ is the amplitude
> - $\omega$ is the frequency
> - $\theta$ is the phase (offset)

# Formula
$$\hat{f}(\omega) = \int_{-\infty}^{\infty} f(x) e^{-2\pi i x \omega} dx$$
- Outputs a complex number for a given frequency $\omega$
	- [[Domains#Number sets|Real]] signals will have no imaginary part

> [!TIP] Euler's Identity For Complex Exponentials
> Using Euler's identity for complex exponentials, the output can be broken down into its real and complex parts
>
>Identity:
> $$e^{2 \pi i \theta} = \cos(2 \pi \theta) + i \sin(2 \pi \theta)$$
>
>Trick:
>$$\hat{f}(\omega) = \int_{-\infty}^{\infty} f(x) \sin (-2 \pi x \omega) dx + i \int_{-\infty}^{\infty} f(x) \cos(-2 \pi x \omega)$$
>Just read off the part that you want

# How it works
1. Take every possible frequency
2. [[maths/Signals#Correlation|Correlate]] them with a  test signal
3. Pick the frequencies that are strongly correlated
	-  The correlation is the amplitude
4. Compare $a[t] = \sin (\omega x)$ and $a[t] = \cos\prime (\omega x) = \sin (\omega x + \frac{pi}{2})$
	- $c(\omega)$ and $c(\omega)\prime$ respectively
5. The phase $\theta = \tan ^{-1} \left(\frac{c(\omega)}{c'(\omega)}\right)$
6. Magnitude without phase: $A = \sqrt{c(\omega)^{2}+ c'(\omega)^2}$

# Inverse
AKA inverse Fourier transform

$$f(x) = \int_{-\infty}^{\infty} \hat{f}(\omega) e^{2 \pi i x \omega} d \xi$$

- Retrieves the original function

# Discrete Fourier transform
$$
X[k] = \sum_{n=0}^{N-1} x[n] e^{-j\frac{2\pi}{N} kn}, \quad k = 0, 1, 2, ..., N-1
$$# Overview
Signals are continuous values that vary over time/space. Usually but not always interpreted as time series. They are used to represent continuous signals modelled as $x_{t} = x(t)$

- We can't get the whole signal with perfect resolution
	- Instead, we sample at a particular rate (measured with some time unit like Hertz, hours, or days)
- The series then undergoes [[#quantisation|amplitude quantisation]] (forcing the signal into a particular range)

# Quantisation
- Sampling at a fixed rate is called "time quantisation"
	- Makes $t$ discrete
- Forcing a signal into a range is called "amplitude quantisation"
	- Makes $f(t)$ discrete
	- Often measured in bits
		- $2^b$ levels - possible values
	- More levels reduces noise but increases resource usage (memory, CPU, storage) and requires more expensive equipment
		- The difference between the actual and quantised values is random

# Sampled signals
- Instead of storing the time of a measurement, sample at a fixed rate and store the signal as a 1D [[Vectors|vector]]
- Spacing: $\Delta T = \frac{1}{f_{s}}$
	- $f_s$ is the sample rate in Hertz

> [!NOTE] Spatial Signals
> Signals can be used for space too. A monitor's signal could be sampled with a number of pixels per inch

# Operations
- Removing offset: vector subtraction
- Mixing signals: [[Vector Operations#Weighted sum|weighted sum]]
- Correlation between signals: vector multiplication

# Noise
- All measurements have noise
	- The model is actually $x(t) = y(t) + \epsilon (t)$
		- $y(t)$ is the true signal
		- $\epsilon(t)$ is the noise
- Signal-to-noise ratio: $SNR = \frac{S}{N}$
	- $S$ is the amplitude of $y(t)$
	- $N$ is the amplitude of $\epsilon(t)$

## Decibels
- A measure of loudness relative to noise
	- Not just for volume!
- $SNR_{dB} = 10 \log_{10}(\frac{S}{N})$

# Filtering
- The process of removing possible noise from a signal
- You need to make assumptions about what $y(t)$ should look like
	- E.G that it changes slowly, so rapid changes can be discarded

## Linear filters
- Linear filtering strateties satisfy [[Matrices#Linearity|linearity]]
- When each output is the [[Vector Operations#Weighted sum|weighted sum]] of the previous inputs
- Efficient at the hardware level

## Moving average
AKA lowpass filter

- Linear filtering strategy
- Average out the signal using a sliding window
	- The output will have $n - K$ samples ($K$ is the size of the window)
		- Because you can't do operations on a window of length $\lt K$
	- Larger $K$ means more smoothing and more high frequencies are squashed
- Exploits the temporal structure of the signal
	- Signals don't have arbitrary changes
- Assumption: noise is independent of the previous step but the true signal is dependent
- Can be defined as a [[#Convolution]] where the convolution kernel is $1N$

## Median filter
AKA order filter

- Nonlinear filter
- Moving average using the median
- Assumption: most samples are good, but some are bad
	- Accounts for large corruptions

## Convolution
$$(x * y)[n] = \sum\limits _{m = -M}^{M} x[n - m] y[m]$$
where for vectors $x[n], y[m]$ of length $N, M$

- The [[Vector Operations#Weighted sum|weighted sum]] of neighbouring values
	- Creates a sliding window that sums with the previous outputs
	- The shape of the window is the weights
- $x$ will be transformed, $y$ is the operation to apply
	- $y$ is called the convolution kernel
	- $M \ll N$
- The general form of all [[#linear filters]]
- [[Properties|Commutative and associative]] properties are defined
	- This means convolutions can be combined into one operation
- Distributive property is defined over addition
	- $f * (g + h) = f * g + f * h$

### Dirac delta
$$
\begin{align*}
&\int_{-\infty}^{\infty} \delta(x) = 1\\
&\delta(x) = \left\{ \begin{array}{ll}
0, x \ne 0\\
\infty, x = 0

\end{array}\right\}\\
\end{align*}$$
- A function that is 0 everywhere **except** 0 = 1
	- [[Vectors|Vector]] representation: a bunch of zeroes with a 1 in the middle
		- The 1 is called the "impulse"
- The [[Identity Property|Identity Element]] element for convolutions
	- Used to retrieve the convolution kernel for a system ("linear system identification")
		- Requires a perfect impulse (the impulse is not blurred or distorted)

> [!NOTE] Impulse Response
> A Dirac delta can be played and the response recorded. This is called the impulse response of the system. You can then multiply the model by the Dirac delta to get back the convolution (such as the reverb in a recording booth)

# Nyquist limit
- If a signal is sampled frequently enough, it can be reconstructed perfectly
- Rule: the frequencies can't exceed $f_{n} = \frac{f_{s}}{2}$
	- Shorthand notations: $f_{n'}, f_{N'}$

## Aliasing
- Happens when you don't sample enough
	- If there is a sampled component with $f_{q} \gt g_{n'}$, there will be an artefact at $f_{n} - (f_{q} \mod f_{n})$
- Anti-aliasing is used in imaging to combat the aliasing caused by reducing the resolution

# Domains
- Signals can be viewed in two ways:
	- A sequence of measurements of amplitude over time: **time domain**
	- Sum of oscillations of different frequencies: **frequency domain**
		- A pure oscillation is a sine wave $x(t) = A \sin (2 \pi \omega t + \theta)$ 
			- $\omega$ is the oscillation frequency
			- $\theta$ is the phase of oscillation (offset)
			- $A$ is the magnitude of oscillation
		- The frequency domains represents signals as the sum of oscillations at all frequencies (with a phase and magnitude assigned to each)
			- If a frequency isn't present, its amplitude would be 0

# Correlation
$$c = \sum\limits_{t} a[t]b[t]$$

- Multiplying two signals ([[Vector Operations#Inner product|inner product]]) gets the correlation
	- How alike the two signals are

| Situation                               | Correlation               |
| --------------------------------------- | ------------------------- |
| Unrelated                               | $c \approx 0$             |
| $a[t], b[t]$ are closely related        | $c$ is large and positive |
| $a[t], b[t]$ are inverses of each other | $c$ is large and negative |
# Overview
This is an index of significant operations that can be applied to [[Arrays|NDArrays]]

# Concepts
## Rigid transformations
- A rigid transformation doesn't change the elements in memory, only the [[Array Representation#Header|headers]] --> $O(1)$ complexity

### List of rigid operations
- [[#Transpose]]
- [[#Rotate]]
- [[#Flip]]

## Rank promotion
Some operations change the [[Tensor Ranks|tensor rank]] of the array. They are sorted into three categories:
- Rank-preserving: the dimensions remain unchanged (E.G mapping, slicing all dimensions)
- Rank-reducing: reduces the number of dimensions (E.G reductions, unravelling)
- Rank-promoting: adds new dimensions

# Transpose
- Each column becomes a row, each row becomes a column
- Swaps the strides in the [[Array Representation#Dope vectors|dope vector]] --> $O(1)$
	- Does not affect the order of the elements in memory

```python
# Using NumPy
A.T
```

# Reshape
- Specify new dimensions and pour the elements into it like molten metal into a mould

```python
import numpy as np

x = np.arange(12)  # Dimensions: (12,)
x.reshape((3, 4))  # Dimensions: (3, 4)
```

## Rules
- The number of elements can't change
- The order of the elements in memory won't change, only the points where the array wraps to the next dimension
- The last dimension changes fastest, the second last changes slower, the third last changes slower still, etc

# Add singleton dimension
- If you have a vector, but need it to behave like a matrix, add a singleton dimension to promote its rank

```python
A = np.array([1, 2, 3])  # (3,)
A[np.newaxis, :]         # (1, 3)
A[:, np.newaxis]         # (3, 1)
```

This can also be done (in a less readable way) with `None` instead of `np.newaxis`

# Squeeze
- Removes [[#Add singleton dimension|singleton dimensions]]
- # Overview
- [[Arrays|NDArrays]] have a very simple representation in memory that allows their operations to be very efficient

# Header
- The header comes before the array contents in memory
- Tells the program how to jump around the array

It contains:
- The number of dimensions
- The number of elements
- The size of each element
- The [[#shape]]
- The [[#strides]]
- A pointer to the array
- Some flags

# Shape
- The dimensions of the array
- E.G `5x4` for 5 rows with 4 columns
# Strides
- Tells you how many bytes to jump to get to the next column or row
- This can be calculated based on the array's [[#shape]] and the size of each element
- This allows jumping to rows and columns to be $O(1)$

Example:
$$
\begin{align*}
cols :&= 4\\
rows :&= 500\\
elementSize :&= 8 \textrm{ bytes}\\
\\
rowStride &= cols \times 8 = 20 \textrm{ bytes}\\
colStride &= elementSize = 8 \textrm{ bytes}
\end{align*}
$$

## Dope vectors
- Holds the striding information
- Two conventions:
	- FORTRAN-style (column-major): `[columnStrides, rowStrides]`
	- C-style (row-major): `[rowStrides, columnStrides]`
	- This defines the default order of iteration through the array

## Illife vectors
- An alternative to strides
- A nested list holding pointers to each row
- Advantage: allows [[Arrays#Definition|ragged arrays]]
- Disadvantage: less efficient
---
aliases:
  - NDArrays
---


# Definition
- Contiguous blocks of memory used for storing homogeneous data
	- All elements must be of the same type
	- This means each element has the same size, which is crucial for allocating the correct amount of memory
- All rows must have the same number of columns
	- The shape is written like so: `ROWxCOLUMN` (E.G `3x2`)
	- An array that has an inconsistent number of columns is said to be "ragged"

# Libraries
## NumPy
- A Python library
- Provides fast array operations
- Built on top of FORTRAN

# Representation
![[Array Representation]]# Overview
- The broadcasting rules are used when applying operations to arrays of mismatching size

See first: [[Arrays]] and [[Tensor Ranks]] 

# Rules
Broadcasting can only happen one of the following is true:
a. The dimensions of the operands are compatible
b. One of the dimensions is `1`

## Compatibility
Two dimensions are compatible if one of the following is true:
a. They are equal
b. One of the dimensions is `1`
c. The last dimensions of the first array match the shape of the second array

# Examples
```python
import numpy as np

a = np.array([1, 2, 3])  # (3,)
b = 2                    # Scalar
a + b                    # 2 is added to each element
```

- `b` implicitly takes on the shape of `a`# Overview
Classification algorithms predict which group an input belongs to based on some parameters

See first: [[Vectors#Feature Vectors]]

# Visualising
- A pair plot is used to plot every dimension against another
- Some comparisons are more useful and will show different groupings

![Pair plot](https://seaborn.pydata.org/_images/pairplot_11_0.png)

# k nearest neighbours
- A classification algorithm
- Uses training data consisting of pairs of $\overrightarrow{x}_{i}, y_{i}$
	- $\overrightarrow{x}_{i}$ is a feature vector
	- $y_{i}$ is a label
- To generate a prediction, the algorithm computes the $k$ nearest neighbours
	- $k$ is constant
- The idea is that the nearest neighbours will share some characteristics---
aliases:
  - Data Science
---
# Overview
Data science is a field that focuses on applying transformations to large datasets such as experimental data, sensor inputs, or media (image/video/audio). [[Machine Learning]] is built on top of this subject.

# Concepts
- [[Vectorisation]]
	- [[SIMD]]
- [[Arrays]]
	- [[Tensor Ranks]]
	- [[Array Representation]]
	- [[Array Operations]]
		- [[Broadcasting]]
- [[Floating Point Representation]]
- [[Data Visualisation]]
- [[The Curse of Dimensionality]]
- [[Linear Algebra Map|Linear algebra]]
- [[Classification]]
- [[Machine Learning]]

# Tools
- [[NumPy Map|NumPy]]# Overview
Patterns in data can be conveyed using images known as "graphs", "charts", or "plot"s

See also:
- [[Layered Grammar of Graphics]]
- [[Plots]]---
aliases: Eigenvectors
---
# Overview
- "Characteristic values"
- There are $n$ eigenvalues for an $n \times n$ matrix
	- These are unique to the matrix
	- The eigenvectors are not unique to the eigenvalue
- The eigenvalues are used in eigenvectors, which are special vectors that only get scaled (no rotation) when multiplied by the matrix
	- $Ax = \lambda x$ where $\lambda$ is the eigenvalue

See also:
- [[Matrix Operations#Eigendecomposition|Eigendecomposition]]

> [!NOTE] Applications
> - Finding the axes of rotation

> [!NOTE] Eigenspectrum
> - The eigenvalues of a matrix can be ordered according to their magnitude
> - Higher eigenvalues are more dominant
> 	- The transformation stretches more in these directions
> 	- If there is a large disparity in eigenvalues, the plot of the data will become a skinny ellipse after repeated applications of $A$
> - They are assigned ranks (0-based, lower is better)

# Algorithms
- [[#Power iteration]]
- [[Matrix Operations#Eigendecomposition|Eigendecomposition]]

## Power iteration
- A method for finding the leading eigenvector
- Repeatedly applying exponentiation and the $L_\infty$ norm
	- $X_{n} = \frac{Ax_{n-1}}{|Ax_{n-1}|_{\infty}}$# Overview
[[Mathematical Optimisation]] build on top of differentiation

> [!QUESTION] Why Higher Order Optimisation
> [[Mathematical Optimisation]] methods are:
> - Slow
> 	- Many iterations required
> 	- Each iteration may be expensive
> - Don't necessarily [[Mathematical Optimisation#Algorithms|converge]]
> - Have too many hyperparameters
> 	- You have to optimise the optimiser
> 
> Unless the problem is [[Mathematical Optimisation#Convex functions|convex]]. Use [[Mathematical Optimisation#Convex optimisation|convex optimisation algorithms]] in that case

# Orders of optimisation
- [[Mathematical Optimisation#Algorithms|Zeroth order methods]] evaluate the [[Mathematical Optimisation#Objective function|objective function]] $L(\theta)$
- First order methods evaluate $L(\theta)$ and $\nabla L(\theta)$
	- See [[#Gradient vector]]
- Second order methods evaluate $L(\theta)$, $\nabla L(\theta)$, and $\nabla \nabla L(\theta)$

# Jacobian matrix
- For a function $y = f(x)$, you can find the Jacobian matrix
	- $x$ is a vector of length $n$
	- $y$ might be a vector. It has length $m$
- The Jacobian is an $n \times m$ matrix containing derivatives $\frac{d f(x)}{dx_{i}}$
- Meanings:
	- Represents the slope at $x$
	- How much each component of $y$ changes when the respective component of $x$ changes
- Particularly useful for functions that are $\mathbb{R}^{n} \rightarrow \mathbb{R}^{n}$ because the Jacobian will be [[Matrices#Square|square]] so [[Matrix Operations|you can apply more operations to it]]

> [!NOTE] Jacobians of Objective Functions
> The Jacobian of an objective function has only one row. This row is known as a [[#Gradient vector]]

## Gradient vector
$\nabla f(x)$ where $x$ is a vector

- A matrix where each component states how much the output of $f(x)$ will change when its respective component is changed
	- For $f : \mathbb{R}^{n} \rightarrow \mathbb{R}$ (a scalar-valued function), $\nabla f(x) : \mathbb{R}^{n} \rightarrow \mathbb{R}^n$
	- For $f : \mathbb{R}^{n} \rightarrow \mathbb{R}^m$ (a matrix-valued function), $\nabla f(x) : \mathbb{R}^{n} \rightarrow \mathbb{R}^{n \times m}$
- Points in the direction of the steepest change in $f(x)$
- The magnitude $|\nabla f(x)|$ is the steepness

# Hessian matrix
$\nabla^{2} f(x)$ where $x$ is a vector

> [!INFO] Key Information
> The second derivative of a function represents its curvature

- The Jacobian of a gradient vector
	- $\nabla^{2} f(x) : \mathbb{R}^{n} \rightarrow \mathbb{R}^{n \times m}$ for scalar-valued functions
	- Outputs a [[Tensor Ranks|tensor]] for vector-valued functions
- Each component $\theta_i$ states the rate change in steepness of all other components $\theta_j$
	- I.E it describes the change in steepness to move in a different direction from $\theta_i$
- Similar to the [[Matrices#Covariance|covariance matrix]] - it's a matrix representing how gradients vary with each other
	- One component for each pair of dimensions

> [!TIP] Geometric Intuition
> Think of a surface. The [[#gradient vector]] is the normal on that surface for a given point
>
> <img src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a5/Normal_vectors2.svg/1200px-Normal_vectors2.svg.png" alt="A normal on a plane" height="200px" />
> 
> The Hessian is a quadratic function that follows the curvature of that surface

## Eigenvalues of Hessian Matrix
- The eigenvalues show what type of [[Mathematical Optimisation#Critical points|critical point]] is at the point

| Eigenvalue Conditions                            | Point Classification | Note                        |
| ------------------------------------------------ | -------------------- | --------------------------- |
| All positive eigenvalues (positive definite)     | Minimum              |                             |
| All negative eigenvalues (negative definite)     | Maximum              |                             |
| Mixed signs among eigenvalues                    | Saddle Point         |                             |
| All positive or negative eigenvalues, some zeros | Semidefinite         | Point is a plateau or ridge |

See also:
- [[Matrices#Definite matrices|Definite matrices]]
# Differentiating objective functions
- Differentiating an objective function outputs the slope of the function
- An optimiser can follow that slope to a [[Mathematical Optimisation#Minima|minimum]]
- The objective function has to be at least $C^1$ continuous to use [[#first order methods]]
	- "$f(x)$ is $C^1$" means that $f\prime(x)$ is continuous
	- This implies that the function has to be differentiable as well
		- The gradient is defined for the entire function (no gaps)

## Lipschitz continuity
Higher order *continuous* optimisers require the objective function to be Lipschitz continuous

Requirements for Lipschitz continuity (for $\mathbb{R}^{n} \rightarrow \mathbb{R}$ functions):
1. The gradient is bounded
2. There is a maximum steepness
	- $\frac{dL(\theta)}{di} \le K$ where $K$ is fixed and $i$ is an index

Requirement 2 can be visualised by plotting an hourglass for each point and requiring that it never overlaps with the function
![Requirement 2 visualisation](https://upload.wikimedia.org/wikipedia/commons/5/58/Lipschitz_Visualisierung.gif)

> [!QUESTION] Why Is This Required?
> You usually can't solve the derivative, but you can evaluate the derivative for a given point ("exact pointwise derivatives"). In order to use this in optimisation, you need to be able to evaluate the gradient at any given point in the function

### Lipschitz constant
Formula for $K$:
$$K = \sup \left[\frac{|f(x) - f(y)|}{|x- y|}\right]$$
- Smooth functions have small $K$s
- Flat functions have $K = 0$

> [!NOTE] Supremum
> $\sup$ is the supremum function: gets the smallest value greater than all values of the function
> 
> ![Supremum diagram](https://simomaths.files.wordpress.com/2012/10/minmaxetc.png)
> 

## Numerical differentiation
- Using the differentiation formula to evaluate the gradient at a given point
	- $\frac{d}{dx} f(x) = \lim_{h \rightarrow 0} \frac{f(x + h) - f(x - h)}{2h}$
	- Just evaluate $L(\theta + h)$ and $L(\theta - h)$ (the "finite differences")
- $h$ will be a minuscule constant. Picking a value that won't result in lots of error is difficult
	- Additionally, there will be high error due to [[Floating Point Representation#Round-off error|floating point round-off]]
- This breaks down in higher dimensions
	- $O(2d)$ evaluations of $L(\theta)$

See also:
- [[The Curse of Dimensionality]]
# First order methods
## Gradient descent
- Starts from a random point $\theta^{(0)}$ and takes steps in the direction of the gradient
	- $\theta^{(i + 1)} = \theta^{(i)} - \delta \nabla L(\theta^{(i)})$
		- $\delta$ is the step size
- Prone to getting trapped in local minima
	- Improvements: random restart and [[#momentum]]
- Only works on smooth functions
	- [[Mathematical Optimisation#Stochastic relaxation|Stochastic relaxation]] (adding randomness to the chosen trajectory) can help with this

> [!TIP] Visual Intuition
> - Gradient descent uses an attractor that rolls towards the solution (a minima)
> 	- Visualise it as a ball rolling down a slope until it settles into a minima
> 
> ![Gradient descent diagram](https://miro.medium.com/v2/resize:fit:1024/1*G1v2WBigWmNzoMuKOYQV_g.png)

> [!NOTE] Tuning Step Size
> - Step size too small: [[Mathematical Optimisation#Algorithms|convergence]] will be slow
> - Step size too large: unpredictable behaviour when there are large changes in gradient *within a step*
> 	- Example: if the gradient changes sign 
> - If you know the [[#Lipschitz constant]], you can find an optimal step size
> 	- This is rarely the case

### Momentum
- Add velocity $v$ and momentum $\alpha$ terms to the equation
	- $v = \alpha v + \delta \nabla L(\theta)$
		- Outputs the velocity for the next run
	- $\theta^{(i + 1)} = \theta^{(i)} -v$
- $\alpha \in [0..1]$ and is constant
	- $\alpha = 0.0$ is just normal gradient descent

## Stochastic gradient descent
AKA SGD

- Use a subset ("minibatch") of the dataset to calculate the dataset
	- Improves performance when the objective function is costly
	- Does reduce accuracy
	- Doing multiple runs smooths the function by providing average results
		- One run through the whole dataset is called an "epoch"
- Particularly effective when the objective function can be represented as a sum of sub-problems
	- $L(\theta) = \sum\limits_{i} L_{i}(\theta)$
	- Common in [[Mathematical Optimisation#Approximation|approximation problems]]

> [!success] Advantages Over Gradient Descent
> - Less data passed to the objective function --> it evaluates faster
> - Reduced memory cost and better cache locality
> - Reduces chance of getting stuck in local minima
> 	- Random sampling adds noise
> 	- This noise increases the likelihood of sub-optimal decisions, reducing the greed --> increased [[Mathematical Optimisation#Algorithms|exploration]]
> 	- This [[Mathematical Optimisation#Stochastic relaxation|stochastic relaxation]] smooths the function, so a function that doesn't meet [[#Lipschitz continuity]] might become Lipschitz-continuous
> - More parallelisable

# Second order methods
- Uses the [[#Hessian matrix]] to go directly to the bottom of local quadratic approximations (I.E [[Mathematical Optimisation#Minima|maxima/minima]])
- Skips [[Mathematical Optimisation#Critical points|flat plains and saddle points]]
- Way faster than [[#first order methods]]
- Works for low-dimensional models only
	- Don't work well in high dimensions due to the space and time [[Time Complexity]] (both $O(d^2$)
	- Solution: approximate the Hessian
	- See also: [[The Curse of Dimensionality]]# Overview
This is one system for describing types of [[Data Visualisation]]s

Visualisations are formed of the following components:
- [[#Stats]]: computed statistics
	- Mean
	- Standard deviation
- Mapping: some function that takes data and creates visual values
	- Contains a scale (E.G a function that converts any number to the range 0..1)
	- Should contain a [[#guide]] - visual tells for what and how data is being mapped
		- Axis ticks
		- Legends
		- Axis labels
- [[#Geom]]: a geometric representation of the mapped data
	- The bar in a bar chart
	- A point in a scatterplot
- Coord: the coordinate system. Geoms and guides depend on this
- Layer: a layer contains some geoms using a mapping and a coordinate system
- Facet: a separate coordinate system, used to display the same data in a different way
- Figure: figures contain facets
- Caption: a short explanation of the visualisation

# Guide
- Axes should be labelled
	- Units should be included
- Axis ticks should be used
- Legends are useful for describing the relationship between layers
	- E.G what the red vs blue lines are
- Should have a title
- Grids can be helpful
	- Shows the coordinate system
- Annotate important features

## Plotting variables
- It's conventional to put variables on particular axes
	- X-axis: independent variable
	- Y-axis: dependent variable
## Axes
-  Avoid non-zero axis origins unless it makes the graph much clearer
	- Starting from a non-zero value can make values look bigger

# Geoms
- A few types
	- Lines/curves
		- Line graph
		- Tells you what happens between points
	- Markers/points
		- Scatter plot
	- Patches
		- Bar charts

## Colour
- Use a colour scale monotonically varying brightness
	- Colours that are *perceptually* equal in difference
	- Human perception of colour is non-linear
- For signed data (can be positive or negative), use a different colour on each side of 0
- Always provide a colour bar for reference


## Showing uncertainty
- Method 1: show an area around a line plot
	- Mark each datapoint
	- Draw a line between them
	- Add an area geom for the range of uncertainty
- Method 2: error bars

![Error bars vs ribbon](https://images.squarespace-cdn.com/content/v1/5a07c63e18b27d4c1ef5e91d/1511639238973-23RAF1PUG5Z98IAOIMM2/before_and_after_error_bands.png)

Error bars (left), ribbon (right)

# Stats
- 3 main types
	- Aggregates
		- Averages like means and medians
		- Deviations like standard deviation, min/max, interquartile range
	- Binning: grouping continuous data into discrete bins
	- Smoothing, regressions: finding a function that approximates the data
		- [[Linear Regression]]
- # Overview
Linear algebra is the field of vectors and matrices

# Concepts
- [[Vectors]]
	- [[Vector Spaces]]
	- [[Vector Operations]]
		- [[Norms]]
- [[Matrices]]
	- [[Matrix Operations]]
- [[Mathematical Optimisation]]
	- [[Higher Order Optimisation]]# Overview
Machine learning is used to solve problems that are so multi-faceted to the degree that they are near-impossible to write algorithms for (E.G image classification, natural language parsing, animating pictures). Instead, a model is trained on a large dataset and it figures out which parameters to use to solve the problem. Machine learning models are typically focused on a single problem.

- The goal is to find a function that approximates the correct mapping from an unseen input to a correct output
- This function will be [[Mathematical Optimisation|optimised]] to minimise the difference between expected and actual outputs

See first: [[Mathematical Optimisation]]

# Deep neural networks
- A neural network is a series of transformations that convert input(s) to output(s)
	- There can be many inputs, outputs, and layers
	- Some parameters will be reduced away by layers
- Each layer is known as a weight matrix
- A squashing function is applied between each layer to keep values within a certain range
	- The same function for all layers

![Neural network diagram](https://miro.medium.com/v2/resize:fit:1400/1*KHs1Chs6TCJDTIIQVyIJxg.png)

## Backpropagation
- The weight-squash-weight set-up allows you to differentiate the objective function w.r.t the weights
- Outputs how much each weight impacts the prediction
	- For the entire neural network! Only one step required

> [!QUESTION] How Does This Work?
> - Concatenate all weight matrices into a vector
> - Calculate gradient of the objective function w.r.t the vector# Overview
The goal of mathematical optimisation is optimising towards a goal given a set of parameters. [[#Parameters]] are passed to the [[#objective function]] until a minimal or maximal value is found \[within the constraints]

See first:
- [[Vector Spaces]]
- [[Vector Operations#Norms|Vector norms]]

See also:
- [[Higher Order Optimisation]]

# Parameters
- Parameters ($\theta$) exist in [[Vector Spaces|parameter space]] as vectors

# Objective function
AKA loss function, cost function

- A function that maps the [[#parameters]] to a single [[Tensor Ranks#Scalars|scalar]]
	- Measures how good a set of parameters is
- Optimisation aims to minimise the objective function
	- Maximisation problems can be converted to this form by negating the objective function
- It may be expensive to evaluate the objective function
	- Could be a real-world experiment
	- Could be dangerous
	- Could require purchases
	- Could require computational resources you don't have
- Optimisation algorithms should evaluate the objective function few times

## Distance functions
$$L(\theta) = ||y\prime - y|| = ||f(x; \theta) - y||$$
- Objective functions that measure the distance between the desired and actual outputs
	- $\theta$ is adjusted by the algorithm (as usual)
	- $x$ is an input (not adjusted by the algorithm)
- Used in [[#approximation]]

# Approximation
- When you have a target and want to get as close as possible to it
- Application: [[Machine Learning]]
- Typically involves training with a large dataset of examples

# Spaces
- [[#Parameters|Parameter spaces]] may be continuous or discrete
	- Continuous optimisation problems tend to be easier because smoothness and continuity can be exploited

# Contraints
- Contraints define the subset of parameters that are feasible
	- E.G if you have a money parameter, $40 quadrillion is unlikely to be feasible
- Shrinks the dimensionality of the set of possible parameters
- They form a "feasible region" of allowed parameters

## Constraint types

> [!NOTE] Equality constraints
> This type constrains the parameters to a surface, representing a tradeoff. For example: number of employees vs time to market

> [!NOTE] Inequality constraints
> This type constrains the parameters to a volume, representing the bounds of the values. For example, a budget constraint

> [!EXAMPLE] Equality vs inequality constraints
> The top graph is an equality constraint and the bottom is an inequality
> 
> ![Equality vs inequality constraint visualisation](https://ars.els-cdn.com/content/image/3-s2.0-B9780123813756000024-f02-02-9780123813756.jpg)

> [!NOTE] Box constraints
> A requirement that $\theta$ lies within a range per dimension
> 
> ![Box constraint visualisation](https://www.alglib.net/optimization/i/asaexample.gif)

> [!NOTE] Convex constraints
> - A set of requirements specified using inequalities 
> - More versatile than box constraints
> 
> ![Convex constraint visualisation](https://www.solver.com/sites/default/files/convexchord.gif)

> [!NOTE] Non-convex problems
> May have multiple minima (points where the objective function is minimised)

## Handling constraints
There are a few methods of applying constraints to a function

### Hard constraints
- Don't allow values outside of the constraints
- Not supported by all optimisation algorithms
- It's not always possible to specify the feasible region using the parameters provided to the optimiser
	- The constraints might involve an intermediate variable
- The result is guaranteed to satisfy the constraints
- Can be more efficient

### Soft constraints
- Use a penalty function to penalise parameters that fail to meet the constraints
	- $L(\theta\prime) = L(\theta) + \lambda(\theta)$
- Supported by all optimisers
- Not applicable to all constraints
	- Sharp constraints
	- Hard to translate constraint to soft constraint

### Relaxation
- Relaxation is the process of relaxing the constraints to find similar continuous or unconstrained problems to solve
- Used by some optimisers to handle discrete or constrained problems
- Replacing hard constraints with penalty functions may also be used

# Minima
- Minima are points in objective function space where the objective function increases in every direction
- Not necessarily the global minimum
	- Local minima can cause optimisers to get stuck on sub-optimal solutions

# Critical points
- Plateaus: causes wandering (optimisers that don't have memory) or stopping (derivative-based optimisers)
	- Mitigated using memory and momentum
- Saddle points (valleys): traps [[Higher Order Optimisation#Gradient descent|gradient descent]] methods
- Ridge: prevents exploration of a portion of the topography

![Objective function line plot](https://www.cs.iusb.edu/~danav/teach/c463/landscape.gif)

![Plotted objective functions](https://preview.redd.it/gias0iu724g81.png?width=918&format=png&auto=webp&s=50178de56c269c4c03bf005a2f791f65a6598d9c)

# Convex functions
- An objective function that has only one minimum is known as a "convex" function
- The optimiser can stop as soon as a minimum is found
- If the optimiser can prove that there is no minimum, it can also terminate

![Convex vs non-convex plot](https://miro.medium.com/v2/resize:fit:793/1*2I9Pf2hrF_-qVzfG23R7Bg.png)

> [!TIP] Classifying Convex Functions
> Quadratic functions are always convex because they form a parabola
>
>![Quadratic function plot](https://mathshistory.st-andrews.ac.uk/Curves/Parabola/Parabola01.gif)

## Convex optimisation
- If the objective function is convex and there are some convex constraints, it can be solved using convex optimisation
- Much more efficient methods
- You have to use iterative methods for non-convex problems
	- Slower
	- Workaround: approximate non-convex problems with convex versions

| Condition                                            | Algorithm type                                                          |
| ---------------------------------------------------- | ----------------------------------------------------------------------- |
| Constraints and objective function are linear        | Linear Programming                                                      |
| Quadratic objective function with linear constraints | Quadratic Programming                                                   |
| Some cases                                           | Semi-quadratic programming, quadratically constrained quadratic program |

# Continuity
- Continuous objective functions are easier to work with
- Local search algorithms fail for discontinuous objective functions

# Algorithms
There are many optimisation algorithms

Key metrics:
- Exploration: how much of the solution space does the algorithm cover
- Exploitation: how much detail can it capture (I.E how close will it get to the optimal solution)
- Convergence: if and how fast the optimiser finds the global minimum
	- In non-convex problems, getting stuck quickly also counts

Potential issues:
- Slow progress: very small changes between steps, causing the objective function to be evaluated many times. Won't cover much space
- Diverging performance: optimisers can get caught in regions of infinitely decreasing values
- Noise: bouncing around without making progress
- Steep/discontinuous objective functions: gradient descent gets stuck. Stochastic methods have to be used


> [!NOTE] Hyperparameters
> - Parameters for the optimisation algorithm
> - Will affect the results of the algorithm for the same parameters
> - E.G the number of rows and columns for Grid Search

## Direct algorithms
### Linear least squares
- Finds a solution directly (not iteratively)
- Handles objective functions of the form $L(x) = ||Ax - y||^{2}_2$
	- Functions that minimise the squared $L_{2}$ norm by applying some [[Matrices|Matrix]]
- The basis for [[Least Squares Regression]]

### Line fitting
- Direct algorithm
- Only works when the objective function is 
- Objective function form: $L(\theta) = \sum\limits_{i}(y_i - mx_{i} + c)^2$
	- Using known points of the form $[x_i, y_i]$
- Built on top of [[Linear Regression]]

## Iterative algorithms
- Repeatedly adjusting the parameters until some termination criteria are met
- Employs termination criteria

General structure:
1. Pick a starting point $x_0$
2. Repeatedly:
	1. Change the parameters
	2. Record the parameters if the objective function was better than the previous results
3. Return the best parameters

### Grid Search
- Brute force algorithm
- Works for continuous and discrete problems
- Requires no knowledge about the parameter space and objective function
- Easy to implement
- Scales poorly with the number of dimensions
- Biased towards the "early corners" of the solution space
- Used for optimising hyperparameters
	- For problems where finding the global minimum doesn't matter as much (E.G [[Machine Learning]])

Structure:
- Divide the feasible parameterset in each direction
	- Fixed number of divisions per dimension (a hyperparameter)
- Use these divisions as samples
- Find the best parameters from the samples

> [!NOTE] Accuracy
> The true minimum will fall between the divisions, therefore the number of divisions determines the accuracy. This is particularly true for objective functions that aren't smooth

### Random Search
- Use a sampling function to pick random parameters
- Can't get trapped in local minima
	- Doesn't use the local structure at all
- Inefficient
- Can't guarantee that the output solution is the best one
- Unpredictable running time

Structure:
1. Generate a random $\theta$
2. Store the parameter if it beats the previous best parameter
3. Terminate after a certain number of iterations since the last change in the best parameter

> [!NOTE] Stochastic optimisation
> Random search is a *stochastic* algorithm, meaning that it employs randomness. Randomness causes non-deterministic running times

> [!NOTE] Heuristics That Affect Performance
> - Locality: similar $\theta$s generate a similar $L(\theta)$. Allows the optimiser to explore nearby values to continue improving
> 	- Only relevant for continuous objective functions
> - Temperature: the rate of movement. Higher temperatures allow the optimiser to cover more space, lower temperatures explore more thoroughly
> - Population: using multiple instances of the algorithm in parallel and comparing the results
> - Memory: remembering good/bad decisions to prevent getting stuck in local minima

### Local Search
- Makes incremental changes to the solution
- Solutions are local to the initial parameter
- Requires a continuous objective function
	- The function should also be smooth
- Prone to getting stuck in local minima
- Used for non-convex problems

### Hill Climbing
- Modification of Random Search applying Local Search
	- Either by adjusting one parameter at a time or by randomly adjusting the whole $\theta$
- Assumes there is some topology in the parameter space ==> there is a neighbourhood around each vector
	- Uses a neighbourhood function to classify neighbours
- Do local search within neighbourhoods of parameters
- Much faster than Random Search
- Assumes the objective function is continuous
- Can get trapped in minima
- Doesn't work with objective function regions that are flat (think of these as valleys - it takes many strides to get out of a valley, which may be larger than the jumps the algorithm is configured to use)

> [!TIP] Handling Local Minima
> Two adjustments can be added to avoid getting stuck in local minima:
> 1. Adaptive local search: if there is no improvement in $n$ steps, make a large jump or increase the step size
> 	- Helps the optimiser escape valleys
> 2. Restart: restart the algorithm multiple times to get different start points. Pick the best result

#### Simulated annealing
- Uses a temperature schedule to adjust the temperature as the optimiser runs
	- Higher temperature at the start, reduce later
- Causes more bad decisions initially, but more likely to overcome ridges and escape local minima
- Based on the *temperature* meta-heuristic

### Genetic algorithms
- A class of optimisation algorithms that mutate, select, and interchange solutions
	- Mutation: add random variation
	- Selection: select the best solutions by culling based on some criteria
	- Interchange: combine multiple solutions to get potentially better ones (based on the idea of breeding)
- Based on the *population* meta-heuristic
	- Maintains multiple potential solutions and continuously compares them
- Works for discrete and continuous problems
- Very flexible
	- Lots of hyperparameters that can be tuned
	- Can also be a problem: picking the right parameters might be difficult
- Requires little knowledge of the objective function
	- But not as efficient as optimisers that use more knowledge
- Evaluates the objective function many times
- Not guaranteed to converge

### Ant colony optimisation
- Maintains a set of $\theta$s that remember their paths
	- A trail of marker vectors is left behind
	- Markers expire over time to avoid getting stuck in a portion of space
- Built on top of the population and memory meta-heuristics
- Best suited for path/route finding
- Handles large, narrow valleys well
- Uses the objective function less than genetic methods
- Lots of hyperparameters
- Not guaranteed to converge

## Selecting algorithms
| Situation                                                           | Recommendation                                                                             |
| ------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ |
| Problem is least-squares                                            | Use a specialized least-squares solver. Pseudo-inverse?                                    |
| Problem is convex                                                   | Use a convex solver.                                                                       |
| Know the derivatives of the objective function, or can compute them | Use a first-order method (or second order, if possible).                                   |
| Don't know any of these things                                      | Use a general-purpose zeroth-order solver like simulated annealing or a genetic algorithm. |

# Stochastic relaxation
- Using controlled randomness to escape local minima

> [!EXAMPLE] Examples
> - Adding noise to the objective funciton
> - Randomly bumping the search trajectory
> - Random sampling

---
tags:
  - Matrix
  - LinearAlgebra
aliases:
  - Matrix
---
# Overview
- Tables / 2D arrays
- They can be used as functions of [[Vectors]]
	- [[Matrix Operations#Multiplication|Multiplication]] by a matrix is like applying a function to a vector
- Two main abilities:
	- Rotation
	- Scaling
	- The combination of rotation and scaling is called "shearing"
- Can also be used as weightings
	- Common in [[Machine Learning]]

# Linearity
Only functions with this property can be written as matrices
- Straight lines remain straight
- Parallel lines remain parallel
- The origin doesn't move
- $A(x + y) = Ax + Ay$
- $A(cx) = cAx$

## Transforms
- Linear maps are functions $f: R^{m} \rightarrow R^{n}$ that maintain the linearity property
- If the map is $n \times n$, it will convert from the same vector space
	- Called a "linear transform"

## Projections
- If $Ax = AAx$, it's called a "linear projection"
- Applying it twice is the same as applying it once

# Definite matrices
- Matrices can be definite, semi-definite, or indefinite
	- Positive or negative

| Property               | Definition                                           |
|------------------------|------------------------------------------------------|
| Positive Definite      | All eigenvalues are greater than 0.                  |
| Negative Definite      | All eigenvalues are less than 0.                     |
| Positive Semi-definite| All eigenvalues are greater than or equal to 0.       |
| Negative Semi-definite| All eigenvalues are less than or equal to 0.          |
| Indefinite             | No other criteria met.                                |

![Graphed definite matrices](https://brickisland.net/cs177/wp-content/uploads/2011/11/ddg_definiteness.svg)

![Graphed definite matrices including positive](https://gregorygundersen.com/image/definiteness/definiteness.png)

> [!NOTE] Dot Products & Definitiveness
> - If the matrix is positive definite, $x^{T}Ax > 0$
> - This means that the [[Vector Operations#Inner product|dot product]] will be positive
> - $x$ won't be rotated by more than $90\degree$

# Linear systems
- A matrix can be written as weights (coefficients) of some variables, resembling a linear system of equations
- They can be solved using [[Matrix Operations#Inverse|inverses]] or [[Mathematical Optimisation]]
	- The inverse method [[Matrix Operations#Inverse|suffers from numerical instability]]

> [!EXAMPLE] Solution Via Inverse
> $$
> \begin{align*}
A&=\begin{bmatrix}\\
0.9 & 0.0 & 0.0 & 0.2 & 0.0 & 0.05 & 0.08 & 0.0
\end{bmatrix}\\
\text{Corresponds to }&\\
y_{1} &= 0.9x_1 + 0.2x_{4} + 0.05x_{6} + 0.08x_7\\
\\
&\text{To solve, left-multiply by the inverse}\\
Ax &= y\\
&= A^{-1}Ax\\
&= A^{-1} \frac{y}{x}\\
&= A^{-1} yx\\
&= A^{-1}y
\end{align*}
> $$

## Overdetermined systems
- A system is overdetermined if it has more inputs than required outputs
- They can be solved using a [[Matrix Operations#Pseudo-inverse|pseudo-inverse]]

$$A = X^+ Y$$
where $X,Y$ are input matrices

# Special matrices
## Diagonal
- Diagonal matrices have values along its diagonal and zeroes elsewhere
- Often used for computations because multiplying by them costs only $O(n)$ instead of $O(n^3)$

## Identity
- A square [[#diagonal]] matrix with 1s on the diagonal
- Has no effect when applied to another matrix or vector
- Denoted by $I$

## Zero
- A matrix of zeroes
- Collapses a matrix to the origin

## Orthogonal
- A matrix where the rows and columns are perpendicular to each other
- The vectors must be [[Vectors#Unit vectors|unit vectors]]
- Transforms a cube to a cube
	- Pure rotation, no scaling
- All of the [[#Eigenvalues|eigenvalues]] are $\pm 1$

> [!NOTE]
> They're referred to as "orthogonal" vectors instead of perpendicular vectors because orthogonality is the more general term. Perpendicularity is only defined in 2 dimensions, whereas orthogonality refers to any number of dimensions

## Covariance
- Denoted by $C$ or $\Sigma$
 - A matrix that represents the spread of the points in a matrix
	 - A higher-dimensional version of variance
 - Computes the variance of each vector from the [[Vector Operations#Mean|mean vector]]
 - The covariance ellipse can be plotted to show a circle that has been sheared to cover all the points
	 - Strongly correlated data will form an ellipse
	 - Weak or uncorrelated data will form a circle

### Inverse covariance matrix
- Use [[Matrix Operations#Singular value decomposition|SVD]] to get the singular values $\Sigma$, then $\frac{1}{\sqrt{\sigma}}$

## Square
- A matrix of shape $n \times n$
- Represents a transformation within a vector space $R^{n} \rightarrow R^n$
- Has many important properties
	- Inverse
	- Determinant
	- [[#Eigendcomposition]]

## Triangular
- A matrix with non-zero elements above or below the diagonal (including the diagonal)
- Either upper-triangular or lower-triangular

## Stochastic
- An [[Graph Algorithms|adjacency matrix]] for a [[Directed Graph]] where the total outgoing weight is 1
	- The sum of each row is 1
	- This means that the inputs and outputs of the graph are balanced
- Preserves mass under flow
- [[Vector Operations#Norms|L1 norm]] = 1

### Doubly stochastic
- Both the rows and columns sum to 1
- Flow is reserved in both directions
---
tags:
  - Matrix
  - LinearAlgebra
---
# Multiplication
- $(R^{n \times m}, R^{m \times p} \rightarrow R^{m \times p}$
- The weighted sum
	- Element-wise multiplication of each row of $A$ by each column of $B$
- Not [[Properties|commutative]]
	- You can say "left-multiply" or "right-multiply" to be specific
- $O(n^3)$

![Example](https://miro.medium.com/v2/resize:fit:1400/1*YGcMQSr0ge_DGn96WnEkZw.png)

# Division
- Left-multiplication by the [[#inverse]]
- Undoes the transform

# Exponentiation
- Multiply a matrix by itself
- Effect: repeatedly applies the operation represented by the matrix

$$A^{n} = \prod^{n}_{i=1} A$$

# Decompositions
Matrix decompositions are algorithms for factorising matrices into its components

## Eigendecomposition
- Gets the [[#Eigenvalues]] for a matrix
- The vectors will be ranked (lower is more important)
- Useful for predicting behaviour across a time scale
	- E.G 7 days in the future
- The original matrix can be reconstructed from the eigenvalues and eigenvectors
- A matrix can be approximated using a some of the leading eigenvectors
	- Useful for large matrices

> [!WARNING] Constraints
> - Inaccurate on non-square matrices
> - The matrix has to be [[Matrices#Diagonal|diagonalisable]]

## Singular value decomposition
AKA SVD

$$A = U \Sigma V^T$$
where:
- $A$ is an $m \times n$ matrix
- $U$ is a square unitary matrix ($m \times m$)
	- The columns are left singular vectors
  - $V$ is another square unitary matrix $m \times m$
	  - The columns are right singular vectors
  - $\Sigma$ is a [[Matrices#Diagonal|diagonal]] $m \times n$ matrix containing the singular values
	  - These values are related to [[eigenvalues]], but not the same
	  - Always positive real numbers
	  - Pure scaling

- Factorises *any* matrix
	- In $O(n^3)$ time for [[Matrices#Square|square matrices]]
- If $A$ is real, $U,V$ will be [[Matrices#Orthogonal|orthogonal matrices]]. All rows and columns will have [[Vector Operations#Norms|norm]] = 1
- $U,V$ are [[Matrices#Orthogonal|orthogonal matrices]] and represent pure rotation
- Can be used to simplify a linear map
	- Technically, it finds a linear map that does a similar thing
	- Will have less flexibility

> [!TIP] Memorable Idiom
> The factors returned by the SVD can be remembered as "rotate-scale-rotate"


> [!NOTE] Special Case: Positive Semi-definite Matrix
> - The columns of $U,V$ will be the [[Eigenvalues|eigenvectors]]
> - $\Sigma$ will be the [[eigenvalues]]
>
> See also: [[Matrices#Definite matrices|Definite Matrices]]

### SVD and exponentiation
- $\Sigma$ can be used to compute fractional powers and [[#inverse]]s
	- Only if the matrix is [[Matrices#Square|square]] and symmetric
- $A^{n} = V \Sigma^{n}U^{T}$
- For inverses of non-symmetric matrices: $A^{-1} = V \Sigma^{-1}U^T$

> [!NOTE] Square Root
> The square root of a matrix can be computed from the SVD by computing the elementwise square root of $\Sigma$ and then substituting it into the aforementioned formula

### Rank
- The number of non-zero values in the matrix
- The rank is the number of dimensions in hyperspace that the transform represents
	- Measures the number of dimensions lost in the transform

| Condition                                                   | Rank                                          |
| ----------------------------------------------------------- | --------------------------------------------- |
| All non-zero singular values                                | Full rank                                     |
| Number of non-zero singular values $\ll$ size of the matrix | Low-rank matrix                               |
| Matrix not full-rank                                        | Singular (deficient rank), cannot be inverted |

### Condition number
- The ratio of the largest singular value (from $\Sigma$) to the smallest
- Only defined for full-rank matrices
- Indicates how sensitive the inverse of the matrix is to minor changes
	- Measures how close the matrix is to being [[#Determinant|singular]]

| Property               | Description      | Meaning                                                                                               |
| ---------------------- | ---------------- | ----------------------------------------------------------------------------------------------------- |
| Small condition number | Well-conditioned | Infrequent numerical issues                                                                           |
| Large condition number | Ill-conditioned  | Significant numerical issues (E.G [[Floating Point Representation#Round-off error\|round-off error]]) |

![[Condition Spectrum.png]]

# Principle component analysis
AKA PCA

- The [[#Eigenvalues|]] of the [[Matrices#Covariance|covariance]] matrix are known as the "principle components" of the matrix
	- They indicate the directions of greatest variance
- Length of a PC: $\sqrt{\lambda_i}$
- The direction of a PC is just the eigenvector

![PCA example](https://numxl.com/wp-content/uploads/principal-component-analysis-pca-featured.png)

> [!NOTE] Dimensionality reduction
> - The key use case for PCA is to reduce a dataset to fewer dimensions
> 	- To visualise it
> 	- To simplify a model
> 	- To apply algorithms that only work for low dimensions
> - Multiply the matrix by each PC to project the data to fewer dimensions
> 	- Pick the most important (longest) PCs and discard the others

# Trace
- The sum of the diagonal values of a matrix **or** the sum of its [[#Eigenvalues|eigenvalues]]
	- $Tr(A) = a_{1,1} + a_{2,2} + \dots + a_{n,n}$
	- $Tr(A) = \sum\limits^{n}_{i=1} \lambda_{i}$
- Only defined for square matrices

# Determinant
- Only defined for square matrices
- Measures the change in volume after the linear transform is applied
- Computed by multiplying the [[#Eigenvalues|eigenvalues]]
	- $det(A) = \prod\limits^{n}_{i=1}\lambda_i$
- If $det(A) = 0$, the transformation is lossy and can't be reversed
	- At least one dimension is collapsed
	- This is because at least one eigenvalue is 0
	- These matrices are called "singular"

# Inverse
- Only defined for square matrices
- Creates a new transform that undoes the original transform
- The [[#Singular value decomposition|SVD]] can be used to compute the inverse in some cases

> [!NOTE] Identities
> - $A^{-1}(Ax) = x$
> - $A^{-1}A = I$
> - $(A^{-1})^{-1} = A$
> - $(AB)^{-1} = B^{-1}A^{-1}$

> [!WARNING] Numerical stability
> Inverting matrices is numerically unstable with current algorithms. This is particularly bad with [[Floating Point Representation|floats]]

## Fast inversions
The following types of matrices can be inverted much quicker:
- [[Matrices#Orthogonal|Orthogonal]]
	- Rows and columns are orthogonal unit vectors
	- $A^{-1} = A^T$, so $O(1)$ time
- [[Matrices#Diagonal|Diagonal]]
	- $A^{-1} = \frac{1}{A}$
	- $O(n)$ time
- [[Matrices#Definite matrices|Positive definite]]
	- $O(n^2)$
- [[Matrices#Triangular|Triangular]]
	- $O(n^2)$

>[!WARNING] Sparse Matrices
> Inverting large sparse matrices is dangerous because the inverse will be dense and therefore cost a lot of memory

## Pseudo-inverse
- Approximates the inverse
- Works for non-square matrices
	- Implication: can solve [[Matrices#Linear systems|systems]] where the number of inputs and outputs is different
- Notation: $A^+$
- Built on top of the [[#Singular value decomposition|SVD]]
	- $A^{+} = V \Sigma^{-1}U^T$
	- $\Sigma$ needs to be zero-padded to be square

# Whitening
- The process of removing linear correlations before analysis
- $X^{W} = (X - \mu) C ^{\frac{-1}{2}}$
	- [[Vector Operations#Centring|Centres the data]]
	- Makes the distribution spherical by making the [[Matrices#Covariance|covariance]] = 1

> [!WARNING] Inverse of Covariance Matrix
> A special method is required to compute the inverse covariance matrix
> ![[Matrices#Inverse covariance matrix]]


See also:
- [[Matrices#Covariance|C, the covariance matrix]]
- [[Vector Operations#Mean|Mean vectors]]---
tags:
  - LinearAlgebra
---
# Overview
A norm gets some metric of a vector. There are a variety of norm functions

# Notation
- Norms are used to get the length of a vector
- Notation: $||x||_p$ where $p$ is the type of norm being applied
- $\mathbb{R}^{n} \rightarrow \mathbb{R}_{\ge 0}$
# General
The general form of a norm is:
$$||x||_{p} = \left(\sum \limits_{i} x^{p}_{i} \right)^{\frac{1}{p}}$$

# Euclidean
- AKA $L_2$ norm
- Gets the distance between elements (in the normal sense of the word)

$$||x||_{2} = \sqrt{x_{1}^{2} + x_{2}^{2} + x_{3}^{2} + \dots + x_{n}^2}$$

# Taxicab
- AKA Manhattan norm or $L_1$
- Gets the sum of the absolute values

$$||x||_{1} = \sum\limits^{n}_{r = 1} |x_{r}|$$

# L infinity
- AKA $L_{\infty}$ norm
- Gets the maximum element

$$||x||_{\infty} = max_{i} |x_{i}|$$

# Normalisation
- Making a vector have length 1 (I.E converting them to a [[Vectors#Unit vectors|unit vector]])
- Turns them into a unit vector signifying a direction

$$\frac{1}{||x||_{2}}$$# Definition
- Single Instruction, Multiple Data
- Special CPU or GPU instructions that can apply to multiple registers at once---
aliases: []
---
# Types of structures
## Scalars
- A single value such as `1`

## Vectors
- A 1D [[Arrays|array]]
	- Rank 1 tensor

## Matrix
- A 2D [[Arrays|array]]
	- Rank 2 tensor

## Tensor
- A 3 or more dimensional [[Arrays|array]]
	- Rank 3 tensor
- Imagine them as groupings of matrices# Overview
As the number of dimensions in a vector increases, the data becomes so sparse that points become almost equi-distant. This is because there are so many ways a vector could be different from another that they are unlikely to be similar

![Explanation](https://miro.medium.com/v2/resize:fit:474/1*kyG0iHo5FCLL8-ZTmvpJhw.jpeg)

Points concentrate towards the corners of the hypercube

![Hypercubes](https://miro.medium.com/v2/resize:fit:1400/1*hWVtGBqiRXdnUYoB6k3yoQ.png)

If you put a sphere inside the hypercube, most points will be on the tiny areas of the intersections between the sphere and the cubes' surfaces. This is because the exterior region is larger than the interior region since there are so many cubes

![Sphere inside hypercube](https://lh3.googleusercontent.com/proxy/LCZS2hJ0nTi4YVYgLcBHJlu-0A8_SI0zOQ20nFVdG0XqosAMOW6RsClYSZ4_I29qqkd3ZgSuR0vLaHjOp52yJG2SxkWW4u_WlsxBF7WrphHr3QB3pxxKmWZsJA)# Overview
- Writing functions that can be run in parallel
- These functions are applied to the whole array
	- This means no explicit iteration
		- More readable
		- Iteration is abstracted away from the user
- Uses [[SIMD]] to process data in parallel

# GPUs
- Graphical Processing Units
- They are specialised for array computations
- Not good at doing logic; CPUs are better for this

# Vector spaces
- A vector in this field is a tuple with 3 elements

> [!EXAMPLE]
> $[5, 7, 3] = 5 \times [1, 0, 0] + 7 \times [0, 1, 0] + 3 \times [0, 0, 1]$

> [!NOTE] Remember
> $$
\begin{align*}
i &= [1, 0, 0]\\
j &= [0, 1, 0]\\
k &= [0, 0, 1]
\end{align*}
$$

# Vector operations
## Multiplication by scalar
$ax = [ax_{1}, ax_{2}, \dots ax_{n}]$

## Addition
AKA "vector composition"
$x + y = [x_{1} + y_{1}, x_{2} + y_{2}, \dots x_{d} + y_{d}]$

## Comparison
- Using [[#Norm]]s or [[#Inner product]]s

## Definition
For this field, we think of vectors as points in space. Other fields think about them as arrows

- Topological vs inner product---
tags:
  - Vector
  - LinearAlgebra
---

# Overview
This is a list of operations that are defined for real vectors

# Scalar multiplication
- AKA weighting or scaling
- Multiply each element of the vector by a [[Tensor Ranks#Scalars|scalar]]

$$ax = [ax_{1}, ax_{2}, \dots ax_{n}]$$

# Addition
AKA "vector composition"

$$x + y = [x_{1} + y_{1}, x_{2} + y_{2}, \dots x_{d} + y_{d}]$$

# Weighted sum
- Addition and scalar multiplication at the same time
- The advantage of assigning a scalar factor for each value is that you can make some values more or less significant
- The vectors must have the same number of dimensions

$$[\lambda_{1} x_{1}, \lambda_{2} x_{2}, \lambda_{3} x_{3}]$$
where $\lambda$ terms are scalar values

## Linear interpolation
- AKA LERP
- Outputs vectors in a line between two vectors

$$lerp(x, y, \alpha) = (1 - \alpha)x + \alpha y$$
where:
- $x$ and $y$ are vectors
- $\alpha$ is the position along the line between $x$ and $y$ in the range $[0..1]$

# Comparison
- Performed using [[#Norm]]s or [[#Inner product]]s
- Which one you use depends on the type of distance you desire

![[Vector Spaces#Topological vs inner product]]

# Inner product
- AKA dot product
- Only works for vectors of the same dimension and they must be in [[Vectors#Topological vs inner product|inner product space]]

$$\cos \theta = \frac{x \cdot y}{||x|| ||y||}$$

> [!WARNING] Be careful...
> The output is in *radians*, not degrees

To reverse:
- Use $\arccos$

## Special cases
> [!NOTE] Unit vectors
> Since $||x||$ and $||y||$ will be $1$, $\cos \theta = x \cdot y$

> [!NOTE] For $\mathbb{R}^n$
> There is a simpler formula for vectors in the $\mathbb{R}^{n}$ space
> $\overrightarrow{x} \cdot \overrightarrow{y} = \sum\limits _{i} x_{i} y_{i}$

# Norms
![[Norms]]

# Mean
- An average of a series of vectors
- Gets the geometric centroid of a set of vectors: the centre-point or "centre of mass" 

$$mean(\overrightarrow{x}_{1}, \overrightarrow{x}_{2}, \dots , \overrightarrow{x}_{n}) = \frac{1}{n} \sum\limits_{i} \overrightarrow{x}_{i}$$

## Centring
- You can move an array of vectors to $\mu= 0$ by subtracting the mean vector from each row

---
tags:
  - Vector
  - LinearAlgebra
aliases:
  - Vector
---
# Overview
Vectors are 1D arrays of a fixed length. They can be used to describe many things, such as points in a mesh, fields with direction ("arrows"), or points in $n$D space. In [[Data Science Map|Data Science]], they are thought of as points in space.

# Formal definition
- Ordered tuples 
	- May have a fixed number of fields ("finite-dimensional")
- All fields must be a member of the same [[Domains|set]]

# Vector spaces
![[Vector Spaces#Overview]]
![[Vector Spaces#Notation]]

# Types of vectors
## Unit vectors
- Vectors of length 1
- E.G $[1, 0, 0]$

> [!WARNING]
$[1, 0, 1]$ is not a unit vector because its length is 2

- Unit vectors almost always have a [[Vector Operations#Euclidean|Euclidean norm]] of 1
- Will always be on the unit sphere
![unit sphere](https://wiki.math.ntnu.no/_media/linearmethods/basicspaces/pnormunitsphere.jpg)

## Basis vectors
- Vectors from which all other vectors in the vector space can be formed

> [!NOTE] Remember
> For the vector space $\mathbb{R}^3$, $i$, $j$, and $k$ are the basis vectors
> 
> $$
\begin{align*}
i &= [1, 0, 0]\\
j &= [0, 1, 0]\\
k &= [0, 0, 1]
\end{align*}
$$

> [!EXAMPLE]
The other vectors in the set are just scaled versions of 
>
> $[5, 7, 3] = 5 \times [1, 0, 0] + 7 \times [0, 1, 0] + 3 \times [0, 0, 1]$

# Vector operations
![[Vector operations]]

# Feature vectors
- Data relevant to the model encoded as a vector

> [!EXAMPLE]
> When modelling flowers, the feature vector might look like this:
> `[petal size, number of petals, height, colour, lifespan]`
---
tags:
  - Vector
  - LinearAlgebra
---
# Overview
- A vector space is a set of vectors
	- For my Data Fundamentals class, we are only working with real numbers and usually vectors of 3 elements ($\mathbb{R}^3$)

## Notation
- $\mathbb{S}^n$ where $\mathbb{S}$ is a [[Domains|set]] and $n$ is the number of fields
- $(\mathbb{S}^{n}, \mathbb{S}^{n}) \rightarrow \mathbb{S}$ is a binary function that maps a pair of vectors to a scalar value

## Topological vs inner product
- [[Vector Operations#Norm|Norm]] operations work in *topological vector space*
	- This means that there is a concept of "closeness" between vectors
- [[Vector Operations#Inner product|Inner products]] work in *inner product space*
	- Distance between vectors is talked about in terms of the angle between vectors

# Curse of dimensionality
> As dimension increases, generalisation gets harder *exponentially*

Most applications of [[Vectors]] in [[Data Science Map|Data Science]] use vectors with an unthinkable number of dimensions. This causes a few problems:

- Data-points will concentrate around the corners of the hyperspace
- This means that visualisation methods like binned histograms don't work so well because most bins are empty

---
Storing data like this takes up a huge amount of storage

---
- Algorithms that work well for few dimensions often fail when many dimensions are present

## Sparseness
- This sparseness means that data-points are less likely to be similar to each other because there are more ways to differ
- In fact, with many dimensions, the $L_{2}$ between any two points is almost the same
	- This begins to happen at $d > 5$ and becomes completely crippling at $d = 20$
- The hypercube will have $2^d$ corners
