# Overview
Machine learning is used to solve problems that are so multi-faceted to the degree that they are near-impossible to write algorithms for (E.G image classification, natural language parsing, animating pictures). Instead, a model is trained on a large dataset and it figures out which parameters to use to solve the problem. Machine learning models are typically focused on a single problem.

See first: [[Vectors]]

# Feature vectors
- Data relevant to the model encoded as a vector

> [!EXAMPLE]
> When modelling flowers, the feature vector might look like this:
> `[petal size, number of petals, height, colour, lifespan]`

# Classification
- A pair plot is used to plot every dimension against another
- Some comparisons are more useful and will show different groupings

![Pair plot](https://seaborn.pydata.org/_images/pairplot_11_0.png)

## k nearest neighbours
- A classification algorithm
- Uses training data consisting of pairs of $\overrightarrow{x}_{i}, y_{i}$
	- $\overrightarrow{x}_{i}$ is a feature vector
	- $y_{i}$ is a label
- To generate a prediction, the algorithm computes the $k$ nearest neighbours
	- $k$ is constant
- The idea is that the nearest neighbours will share some characteristics