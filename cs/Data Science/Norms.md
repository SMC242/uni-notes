---
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

$$\frac{1}{||x||_{2}}$$