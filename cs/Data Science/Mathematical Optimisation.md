# Overview
The goal of mathematical optimisation is optimising towards a goal given a set of parameters. [[#Parameters]] are passed to the [[#objective function]] until a minimal or maximal value is found \[within the constraints]

See first:
- [[Vector Spaces]]
- [[Vector Operations#Norms|Vector norms]]

# Parameters
- Parameters ($\theta$) exist in [[Vector Spaces|parameter space]] as vectors

# Objective function
AKA loss function, cost function

- A function that maps the [[#parameters]] to a single [[Tensor Ranks#Scalars|scalar]]
	- Measures how good a set of parameters is
- Optimisation aims to minimise the objective function
	- Maximisation problems can be converted to this form by negating the objective function
- It may be expensive to evaluate the objective function
	- Could be a real-world experiment
	- Could be dangerous
	- Could require purchases
	- Could require computational resources you don't have
- Optimisation algorithms should evaluate the objective function few times

## Distance functions
$$L(\theta) = ||y\prime - y|| = ||f(x; \theta) - y||$$
- Objective functions that measure the distance between the desired and actual outputs
	- $\theta$ is adjusted by the algorithm (as usual)
	- $x$ is an input (not adjusted by the algorithm)
- Used in [[#approximation]]

# Approximation
- When you have a target and want to get as close as possible to it
- Application: [[Machine Learning]]

# Spaces
- [[#Parameters|Parameter spaces]] may be continuous or discrete
	- Continuous optimisation problems tend to be easier because smoothness and continuity can be exploited

# Contraints
- Contraints define the subset of parameters that are feasible
	- E.G if you have a money parameter, $40 quadrillion is unlikely to be feasible
- Shrinks the dimensionality of the set of possible parameters
- They form a "feasible region" of allowed parameters

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
> ![Box constraint visualisation](https://www.alglib.net/optimization/i/asaexample.gif)

> [!NOTE] Convex constraints
> - A set of requirements specified using inequalities 
> - More versatile than box constraints
> 
> ![Convex constraint visualisation](https://www.solver.com/sites/default/files/convexchord.gif)

> [!NOTE] Non-convex problems
> May have multiple minima (points where the objective function is minimised)

## Handling constraints
There are a few methods of applying constraints to a function

### Hard constraints
- Don't allow values outside of the constraints
- Not supported by all optimisation algorithms
- It's not always possible to specify the feasible region using the parameters provided to the optimiser
	- The constraints might involve an intermediate variable
- The result is guaranteed to satisfy the constraints
- Can be more efficient

### Soft constraints
- Use a penalty function to penalise parameters that fail to meet the constraints
	- $L(\theta\prime) = L(\theta) + \lambda(\theta)$
- Supported by all optimisers
- Not applicable to all constraints
	- Sharp constraints
	- Hard to translate constraint to soft constraint

### Relaxation
- Relaxation is the process of relaxing the constraints to find similar continuous or unconstrained problems to solve
- Used by some optimisers to handle discrete or constrained problems
- Replacing hard constraints with penalty functions may also be used

# Minima
- Minima are points in objective function space where the objective function increases in every direction
- Not necessarily the global minimum
	- Local minima can cause optimisers to get stuck on sub-optimal solutions
# Convex functions
- An objective function that has only one minimum is known as a "convex" function
- The optimiser can stop as soon as a minimum is found
- If the optimiser can prove that there is no minimum, it can also terminate

![Convex vs non-convex plot](https://miro.medium.com/v2/resize:fit:793/1*2I9Pf2hrF_-qVzfG23R7Bg.png)

> [!TIP] Classifying Convex Functions
> Quadratic functions are always convex because they form a parabola
>
>![Quadratic function plot](https://mathshistory.st-andrews.ac.uk/Curves/Parabola/Parabola01.gif)

## Convex optimisation
- If the objective function is convex and there are some convex constraints, it can be solved using convex optimisation
- Much more efficient methods
- You have to use iterative methods for non-convex problems
	- Slower
	- Workaround: approximate non-convex problems with convex versions

| Condition                                            | Algorithm type                                                          |
| ---------------------------------------------------- | ----------------------------------------------------------------------- |
| Constraints and objective function are linear        | Linear Programming                                                      |
| Quadratic objective function with linear constraints | Quadratic Programming                                                   |
| Some cases                                           | Semi-quadratic programming, quadratically constrained quadratic program |

# Continuity
- Continuous objective functions are easier to work with
- Local search algorithms fail for discontinuous objective functions

# Algorithms
There are many optimisation algorithms

Key metrics:
- Exploration: how much of the solution space does the algorithm cover
- Exploitation: how much detail can it capture (I.E how close will it get to the optimal solution)
- Convergence: if and how fast the optimiser finds the global minimum
	- In non-convex problems, getting stuck quickly also counts

Potential issues:
- Slow progress: very small changes between steps, causing the objective function to be evaluated many times. Won't cover much space
- Diverging performance: optimisers can get caught in regions of infinitely decreasing values
- Noise: bouncing around without making progress
- Plateaus: causes wandering (optimisers that don't have memory) or stopping (derivative-based optimisers)
	- Mitigated using memory and momentum
- Saddle points (valleys): traps gradient descent methods
- Steep/discontinuous objective functions: gradient descent gets stuck. Stochastic methods have to be used

![Objective function line plot](https://www.cs.iusb.edu/~danav/teach/c463/landscape.gif)

![Plotted objective functions](https://preview.redd.it/gias0iu724g81.png?width=918&format=png&auto=webp&s=50178de56c269c4c03bf005a2f791f65a6598d9c)

> [!NOTE] Hyperparameters
> - Parameters for the optimisation algorithm
> - Will affect the results of the algorithm for the same parameters
> - E.G the number of rows and columns for Grid Search

## Direct algorithms
### Linear least squares
- Finds a solution directly (not iteratively)
- Handles objective functions of the form $L(x) = ||Ax - y||^{2}_2$
	- Functions that minimise the squared $L_{2}$ norm by applying some [[Matrices|Matrix]]

### Line fitting
- Direct algorithm
- Only works when the objective function is 
- Objective function form: $L(\theta) = \sum\limits_{i}(y_i - mx_{i} + c)^2$
	- Using known points of the form $[x_i, y_i]$
- Built on top of [[Linear Regression]]

## Iterative algorithms
- Repeatedly adjusting the parameters until some termination criteria are met
- Employs termination criteria

General structure:
1. Pick a starting point $x_0$
2. Repeatedly:
	1. Change the parameters
	2. Record the parameters if the objective function was better than the previous results
3. Return the best parameters

### Grid Search
- Brute force algorithm
- Works for continuous and discrete problems
- Requires no knowledge about the parameter space and objective function
- Easy to implement
- Scales poorly with the number of dimensions
- Biased towards the "early corners" of the solution space
- Used for optimising hyperparameters
	- For problems where finding the global minimum doesn't matter as much (E.G [[Machine Learning]])

Structure:
- Divide the feasible parameterset in each direction
	- Fixed number of divisions per dimension (a hyperparameter)
- Use these divisions as samples
- Find the best parameters from the samples

> [!NOTE] Accuracy
> The true minimum will fall between the divisions, therefore the number of divisions determines the accuracy. This is particularly true for objective functions that aren't smooth

### Random Search
- Use a sampling function to pick random parameters
- Can't get trapped in local minima
	- Doesn't use the local structure at all
- Inefficient
- Can't guarantee that the output solution is the best one
- Unpredictable running time

Structure:
1. Generate a random $\theta$
2. Store the parameter if it beats the previous best parameter
3. Terminate after a certain number of iterations since the last change in the best parameter

> [!NOTE] Stochastic optimisation
> Random search is a *stochastic* algorithm, meaning that it employs randomness. Randomness causes non-deterministic running times

> [!NOTE] Heuristics That Affect Performance
> - Locality: similar $\theta$s generate a similar $L(\theta)$. Allows the optimiser to explore nearby values to continue improving
> 	- Only relevant for continuous objective functions
> - Temperature: the rate of movement. Higher temperatures allow the optimiser to cover more space, lower temperatures explore more thoroughly
> - Population: using multiple instances of the algorithm in parallel and comparing the results
> - Memory: remembering good/bad decisions to prevent getting stuck in local minima

### Local Search
- Makes incremental changes to the solution
- Solutions are local to the initial parameter
- Requires a continuous objective function
	- The function should also be smooth
- Prone to getting stuck in local minima
- Used for non-convex problems

### Hill Climbing
- Modification of Random Search applying Local Search
	- Either by adjusting one parameter at a time or by randomly adjusting the whole $\theta$
- Assumes there is some topology in the parameter space ==> there is a neighbourhood around each vector
	- Uses a neighbourhood function to classify neighbours
- Do local search within neighbourhoods of parameters
- Much faster than Random Search
- Assumes the objective function is continuous
- Can get trapped in minima
- Doesn't work with objective function regions that are flat (think of these as valleys - it takes many strides to get out of a valley, which may be larger than the jumps the algorithm is configured to use)

> [!TIP] Handling Local Minima
> Two adjustments can be added to avoid getting stuck in local minima:
> 1. Adaptive local search: if there is no improvement in $n$ steps, make a large jump or increase the step size
> 	- Helps the optimiser escape valleys
> 2. Restart: restart the algorithm multiple times to get different start points. Pick the best result

#### Simulated annealing
- Uses a temperature schedule to adjust the temperature as the optimiser runs
	- Higher temperature at the start, reduce later
- Causes more bad decisions initially, but more likely to overcome ridges and escape local minima
- Based on the *temperature* meta-heuristic

### Genetic algorithms
- A class of optimisation algorithms that mutate, select, and interchange solutions
	- Mutation: add random variation
	- Selection: select the best solutions by culling based on some criteria
	- Interchange: combine multiple solutions to get potentially better ones (based on the idea of breeding)
- Based on the *population* meta-heuristic
	- Maintains multiple potential solutions and continuously compares them
- Works for discrete and continuous problems
- Very flexible
	- Lots of hyperparameters that can be tuned
	- Can also be a problem: picking the right parameters might be difficult
- Requires little knowledge of the objective function
	- But not as efficient as optimisers that use more knowledge
- Evaluates the objective function many times
- Not guaranteed to converge

### Ant colony optimisation
- Maintains a set of $\theta$s that remember their paths
	- A trail of marker vectors is left behind
	- Markers expire over time to avoid getting stuck in a portion of space
- Built on top of the population and memory meta-heuristics
- Best suited for path/route finding
- Handles large, narrow valleys well
- Uses the objective function less than genetic methods
- Lots of hyperparameters
- Not guaranteed to converge

## Selecting algorithms
| Situation                                                           | Recommendation                                                                             |
| ------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ |
| Problem is least-squares                                            | Use a specialized least-squares solver. Pseudo-inverse?                                    |
| Problem is convex                                                   | Use a convex solver.                                                                       |
| Know the derivatives of the objective function, or can compute them | Use a first-order method (or second order, if possible).                                   |
| Don't know any of these things                                      | Use a general-purpose zeroth-order solver like simulated annealing or a genetic algorithm. |


# Orders of optimisation
- First order methods have an attractor that rolls towards the solution (a minima)
	- Visualise it as a ball rolling down a slope until it settles into a minima

# Gradient descent
- A method built on top of [[Automatic Differentiation]]