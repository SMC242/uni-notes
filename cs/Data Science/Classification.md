# Overview
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
- The idea is that the nearest neighbours will share some characteristics