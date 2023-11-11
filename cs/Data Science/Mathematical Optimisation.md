# Overview
The goal of mathematical optimisation is optimising towards a goal given a set of parameters. [[#Parameters]] are passed to the [[#objective function]] until a minimal or maximal value is found \[within the constraints]

# Parameters
- Parameters ($\theta$) exist in [[Vector Spaces|parameter space]] as vectors

# Objective function
- A function that maps the [[#parameters]] to a single [[Tensor Ranks#Scalars|scalar]]
	- Measures how good a set of parameters is
- May be also be called: 
	- Loss function
	- Cost function
- It may be expensive to evaluate the objective function
	- Could be a real-world experiment
	- Could be dangerous
	- Could require purchases
	- Could require computational resources you don't have

# Approximation
- When you have a target and want to get as close as possible to it

# Spaces
- [[#Parameters|Parameter spaces]] may be continuous or discrete
	- Continuous optimisation problems tend to be easier because smoothness and continuity can be exploited

# Contraints
- Contraints define the subset of parameters that are feasible
	- E.G if you have a money parameter, $40 quadrillion is unlikely to be feasible
- Shrinks the dimensionality of the set of possible parameters

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
> ![Box constraint visualisation](https://www.quantumcomputinginc.com/wp-content/uploads/2021/05/603e66911c49c9644c734b0a_constrained-optimization.jpg)

> [!NOTE] Convex constraints
> - A set of requirements specified using inequalities 
> - More versatile than box constraints
> 
> ![Convex constraint visualisation](https://www.solver.com/sites/default/files/convexchord.gif)

> [!NOTE] Non-convex problems
> May have multiple minima (points where the objective function is minimised)

## Handling constraints
There are a few methods of applying constraints to a function

### Relaxation
- Relaxation is the process of relaxing the constraints to find similar continuous or unconstrained problems to solve
- One method is to reduce the "hardness" of the constraints:
	- Hard constraints limit the set of allowed constraints
	- Soft constraints apply a penalty to $\theta$s outside the feasible range
		- A penalty function is defined to achieve this

# Algorithms
There are many optimisation algorithms

- Line fitting
	- Only works when the objective function is 
	- Built on top of [[Linear Regression]]
- Iterative
- Grid Search
	- Brute force algorithm
	- Works for continuous and discrete problems
	- Requires no knowledge about the parameter space and objective function
	- Easy to implement
	- Scales poorly with the number of dimensions
	- Biased towards the "early corners" of the solution space
- Random Search
	- Use a sampling function to pick random parameters
	- Can't get trapped in local minima
	- Inefficient
	- Can't guarantee that the output solution is the best one
- Local Search
	- Makes incremental changes to the solution
	- Solutions are local to the initial parameter
	- Assume that the solutions vary smoothly
- Hill Climbing
	- Modification of Random Search applying Local Search
	- Assumes there is some topology in the parameter space ==> there is a neighbourhood around each vector
	- Do local search within neighbourhoods of parameters
	- Much faster than Random Search
	- Assumes the objective function is continuous
	- Can get trapped in minima
	- Doesn't work with objective function regions that are flat (think of these as valleys - it takes many strides to get out of a valley, which may be larger than the jumps the algorithm is configured to use)
- 

## Hyperparameters
- Parameters for the optimisation algorithm
- Will affect the results of the algorithm for the same parameters
- E.G the number of rows and columns for Grid Search

# Orders of optimisation
- First order  methods have an attractor that rolls towards the solution (a minima)
	- Visualise it as a ball rolling down a slope until it settles into a minima

# Gradient descent
- A method built on top of [[Automatic Differentiation]]