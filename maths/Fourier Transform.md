# Overview
A fundamental operation that breaks a function into a sum of sinusoids (functions that use the $\sin(\theta))$ function. These waves sum to create the function

- The function must be periodic (repeating)
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
\begin{align*}
F[k] &= \sum\limits_{j = 0}^{N - 1} x[j]e^{-2 \pi i \frac{j}{n}}\\\\
&= \sum\limits_{j = 0}^{N - 1} \left( x[j] \cos \left(-2 \pi \frac{j}{N} \right) + ix[j] \sin \left( -2 \pi \frac{j}{N} \right) \right)
\end{align*}
$$

- The Fourier transform for discrete values
	- Used for [[Vectors]]
- Outputs $|x[t]|$ elements
	- Each element is a complex number $F[k] = a + bi$
	- The angle $\theta$ on the Argand diagram is the phase
	- The frequency is $freq = \frac{f_{N}k}{N}$
		- $f_N$ is the [[maths/Signals#Nyquist limit|Nyquist rate]]
	- Output is in evenly-spaced sub-divisions
- $k$ frequency components up to $N - 1$
- Expensive to compute ($O(n^2)$)


![Argand Diagram](https://isaacphysics.org/images/content/concepts/maths/figures/Complex_numbers_figW_2.svg)

## Spectra
- Magnitude spectrum of a signal: the magnitude of each component
	- A function of frequency
	- Plotted on a [[maths/Signals#Decibels|logarithmic scale]]
		- Shows relative scale of components
- Phase spectrum of a signal: the phase of each component
	- Plotted on a wrapped scale $\mod 2\pi$
		- Shows relationship between components
- Main lobe: where the majority of the energy is
- Side lobes: pockets of energy in other frequencies (outside the main lobe)
	- Distortion
	- Often a side-effect
- Side lobe level: ratio of energy in side lobes to main lobe energy

## Fast Fourier transform
AKA FFT

- $O(n \log(n))$   if $n$ is a power of 2
	- $O(n^2)$ if not
	- Works by splitting the signal in half repeatedly
- 