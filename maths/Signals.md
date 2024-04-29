# Overview
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
