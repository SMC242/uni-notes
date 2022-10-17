When doing a [[T test|T test]], it can be useful to produce a range that values are likely to fall within. This is called a "confidence interval" or "margin of error". 

Questions for this often read like "Construct a 90% confidence interval...". The intermediate result looks like this: $2.45 \pm 0.4 kg$, which is used to get the range: $(2.05, 2.85)kg$.

# Formula
$$\bar{x} \pm t^*_{df} \times SE$$
where $t^*_{df}$ is the critical value found by [[#Finding the critical value|looking at a T-table]],
and $SE$ is the [[T test#Standard error|standard error]]

# Finding the critical value
Like in a normal [[T test]], you use the [[Hypothesis Testing#Level of significance|level of significance]] and [[Hypothesis Testing#Degrees of freedom|degrees of freedom]] to look up a p value. The bottom row of the table is used for convenience (instead of the top row).

Confidence intervals are always [[T test#Tails|two-tailed]], so a 90% confidence interval has $\alpha = 0.05$
![[T Table.png]]