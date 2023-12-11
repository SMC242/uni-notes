# Overview
Fitts' Law says that its easier to find things that are big and close to where you currently are

![[fitts-law-example]]

# Formula
$$
T = a + b \log_{2}\left(1 + \frac{D}{W}\right)
$$
where
$$
\begin{flalign}
&T = \textrm{time to complete movement}&&\\
&a = \textrm{cognition constant}\\
&b = \textrm{hand-eye coordination constant}\\
&D = distance\\
&W = \textrm{target width}
\end{flalign}
$$

# Difficulty
"Index of task difficulty"
- A metric for task difficulty
- Unit: bits

$$
ID = \log_{2} \left(\frac{D}{W} + 1 \right)
$$

# Performance
"Index of performance/throughput"
- The rate of actions

$$IP = \frac{ID}{T}$$
# Use case
- Predicting movement time
- Comparing input devices
- Placing elements in a view

## Design implications
- Make elements easier to move to by making elements bigger and closer together
- Right-click menus show context actions in-context
- Put things on screen edges
	- Particularly corners
	- Requires less accuracy