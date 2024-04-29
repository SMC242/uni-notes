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
- 