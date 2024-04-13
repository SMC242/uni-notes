# Overview
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
	- See also: [[The Curse of Dimensionality]]