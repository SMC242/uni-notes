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
$$