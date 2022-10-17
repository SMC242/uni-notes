When working with [[Trends#Temporal methods|time series]], it can be useful to break down the model into its trend $m_t$ and seasonal components $s_t$. These components can be thought of as sub-models.

Differencing can be used to remove the trend and seasonal components entirely.

# Models
$Y_t = m_t + s_t + e_t$
where
	$Y_t$ is the response
	$m_t$ is the trend
	$s_t$ is the seasonal component
	$e_t$ is the error

# Harmonic regression
Used for modelling seasonal trends. 
Form: 
$$\beta_0 + \gamma sin(\frac{2\pi(u_t - \theta)}{p})$$
where
	$p$ is the period
	$\beta_0$ is the height of the wave's midpoint (where y=0)
	$\gamma$ is the amplitude
	$2\gamma$ is the range
	$\theta$ is how much the wave is shifted horizontally ("phase angle")
	
Periodic time series models build on this. 
$$Y_t = \beta_0 + \gamma_1 sin(\frac{2 \pi u_t}{p}) + \gamma_2 cos(\frac{2 \pi u_t}{p}) +e_t$$
where
	$Y$ is the response
	$\beta_0$ is the intercept
	$\gamma$ are the coefficients of the harmonic regression terms
	$p$ is the period
	$u$ is the unit of time (month, day, hour, ...)

# Checking assumptions
- See if the points line on a straight line
	- Use a [[Plot Types#QQ plot|QQ plot]]
- Check that the residuals are normally distributed
- Check that there is no structure in the [[Residuals#Residual-fit plot|residual-fit plot]]
	- The residuals should be randomly scattered

# Differencing
First order differencing removes trend $m_t$. Seasonal differencing is for the seasonal pattern $s_t$. The two can be combined.

## First order differencing
$\nabla X_t = X_t - X_{t-1}$
Where $X_t$ is a time series model evaluated.
![[Trends#Temporal methods#Notation]]