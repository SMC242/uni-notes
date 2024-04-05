# Overview
This is one system for describing types of [[Data Visualisation]]s

Visualisations are formed of the following components:
- Stats: computed statistics
	- Mean
	- Standard deviation
- Mapping: some function that takes data and creates visual values
	- Contains a scale (E.G a function that converts any number to the range 0..1)
	- Should contain a [[#guide]] - visual tells for what and how data is being mapped
		- Axis ticks
		- Legends
		- Axis labels
- [[#Geom]]: a geometric representation of the mapped data
	- The bar in a bar chart
	- A point in a scatterplot
- Coord: the coordinate system. Geoms and guides depend on this
- Layer: a layer contains some geoms using a mapping and a coordinate system
- Facet: a separate coordinate system, used to display the same data in a different way
- Figure: figures contain facets
- Caption: a short explanation of the visualisation

# Guide
- Axes should be labelled
	- Units should be included
- Axis ticks should be used
- Legends are useful for describing the relationship between layers
	- E.G what the red vs blue lines are
- Should have a title
- Grids can be helpful
	- Shows the coordinate system
- Annotate important features

# Geoms
- A few types
	- Lines/curves
		- Line graph
	- Markers/points
		- Scatter plot
	- Patches
		- Bar charts

# Plotting variables
- It's conventional to put variables on particular axes
	- X-axis: independent variable
	- Y-axis: dependent variable

# Showing uncertainty
- Method 1: show an area around a line plot
	- Mark each datapoint
	- Draw a line between them
	- Add an area geom for the range of uncertainty
- Method 2: error bars

![Error bars vs ribbon](https://images.squarespace-cdn.com/content/v1/5a07c63e18b27d4c1ef5e91d/1511639238973-23RAF1PUG5Z98IAOIMM2/before_and_after_error_bands.png)

Error bars (left), ribbon (right)

# Axes
- Avoid non-zero axis origins unless it makes the graph much clearer
	- Starting from a non-zero value can make values look bigger
	- 