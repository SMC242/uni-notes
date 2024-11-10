# Arc consistency
- Intuition: thin down the values in the domains before searching
- Usually doesn't solve the problem on its own
- A problem is arc-consistent if every arc is consistent
	- Not true for all problems!

Possible results:
1. Every domain has one value
	- One solution (both the best and worst solution)
2. Some domains have no values
	- The constraints were unsatisfiable
1. All domains have at least one value
	- Multiple solutions

## Constraint networks
- Used to compute arc consistency
- Representing the relationships between constraints and their variables as a [[Undirected Graph#Connectivity|bipartite graph]] 
	- Represent all variables and constraints as nodes
	- Draw edges ("arcs") between constraints and their dependent variables
- Only words for binary constraints

> [!NOTE] Arc consistency definition
> An arc between a variable $X$ and a binary constant $C_{i}(x,y)$ is arc consistent if there is a $y$ for each $x$ that satisfies $C_i$

## Handling inconsistent problems
- If the problem is not arc consistent, you can remove each inconsistent value from a domain
- Remember to check the other domains for arc consistency
	- You don't have to reconsider all the domains

## Time complexity
- Arc consistency always halts because you ultimately end up with empty domains
- Worst case [[Time Complexity]] is $O(c d ^3)$ where $c$ is the number of constraints and $d$ is the maximum [[Set Operations#Cardinality]] of the domains