When evaluating a [[Least Squares Regression|Least Squares Line]], it's important to look at the influence of outliers. Outliers have a different level of influence depending on where they are, relative to the main cluster of data-points

# What is influence?
Influence or *leverage* is how much a point affects the line of best fit. Data-points that are further away from the main group on the X-axis have more leverage. Points with high leverage are called *influential points*. 

# What to do with them?
Do not remove them just because they're inconvenient. Their existence is important to the model because it shows that there are some edge cases.