# Overview
- Writing functions that can be run in parallel
- These functions are applied to the whole array
	- This means no explicit iteration
		- More readable
		- Iteration is abstracted away from the user
- Uses [[SIMD]] to process data in parallel

# GPUs
- Graphical Processing Units
- They are specialised for array computations
- Not good at doing logic; CPUs are better for this