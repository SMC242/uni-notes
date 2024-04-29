# Overview
An [[Markov Chain Monte Carlo|MCMC]] algorithm based on [[Mathematical Optimisation#Simulated annealing|simulated annealing]]

- Uses a proposal distribution $Q(\theta' | \theta)$ that acts as the [[Mathematical Optimisation#Hill Climbing|neighbourhood function]]
	- Usually a [[Normal Distribution]] with mean = $x$ and a preset $\theta$
	- Choosing this distribution is very important!
- Some function $f_{X}(\theta)$ controls the likelihood of taking certain steps
- Wanders around the distribution [[Vector Spaces|space]], accepting/rejecting jumps to new positions
	- The jumps depend on the current position
		- This is why it's called a [[Markov Chain]]

