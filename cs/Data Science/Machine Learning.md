# Overview
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
> - Calculate gradient of the objective function w.r.t the vector