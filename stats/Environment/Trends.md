There are two main categories of trends in Environmental Statistics:
- Time-based 
	- Seasonal patterns
	- Temporal correlation
- Space-based
	- Spatial trends
	- Spatial correlation

# Spatial methods
Maps, such as heat maps, are useful for visualising spatial trends.
![Knotweed heat map|300](https://www.environetuk.com/sites/default/files/s3fs-public/heatmap_uk_2.jpg)

# Temporal methods
A common technique is to record the time along with a measurement. Changes across time can be observed this way. This is called a time series and they are often used to forecast the future.

## Seasonality
Patterns do not have to be doing one thing all the time. They may occur at an interval (seasonally). The time between peaks is called the period.

If there is usually a pattern and it suddenly changes, that is cause for investigation. 

## Visualisation
- Box plots
![[Temporal Box Plot.png|400x400]]
- Scatter plot
![[Temporal Scatter Plot.png|300x300]]
- Time series graph
![[Time Series Graph.png]]

## Notation
Regular interval: $X_1, X_2, X_3, ..., X_t, ..., X_{n-1}, X_n$
	where $t$ is the position in the time series (time index),
	and $n$ is the number of time points
Model: $X = trend + \textrm{seasonal component} + errror$
Shortened model: $X_t = m_t + s_t + e_t$
Error: $e_t = X_t - m_t - s_t$

## Using [[Linear Regression]]
The following assumptions must be checked:
- Observations are independent
	- Think about whether this is reasonable based on the information given
- Random errors $e_t$ have have a mean of 0
	- Make a [[Residuals#Residual-fit plot|residual-fit plot]]
	- Check whether the points above and below 0 are roughly balanced
- Errors $e_t$ have constant variance
	- Also use a residual-fit plot
	- Make sure the vertical variation is roughly constant
- Errors follow a normal distribution
	- Make a plot like this and see if the points roughly fit the line
![[Q-Q Plot.png]]

# Differencing
Instead of using a [[Linear Regression]], it can be useful to inspect the pattern underneath the trend $m_t$ and seasonal component $s_t$. 


# Remote sensing
This covers the use of satellite imaging to take measurements. This works by inspecting the wavelengths of the reflections of objects. Different wavelengths indicate different conditions, like carbon in soil, chlorophyll in water, or temperature.

Lakes are often observed because they are key to the environment and they are sensitive to harm.

## Advantages
- Reaches places that humans can't
- Wider picture than a human could take (spatial coverage)
- Can take measurements at regular intervals

## Disadvantages
- Pricey
- Environmental conditions can interfere 
	- Clouds can block vision
	- Snow can cover ground
	- Ice can form on water
- Liable to malfunctions
