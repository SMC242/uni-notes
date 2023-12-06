---
tags:
  - Strings
---
# Overview
There are various methods for comparing string similarity

See first: [[String Notation]]

## Formal definition
Given $s = s_{0}s_{2}..s_{m-1}$ and $t=t_{0}t_{2}..t_{n-1}$, what is the least number of operations required to convert $s$ to $t$

# Operations
- Insert: add a character
- Delete: remove a character
- Substitute: replace a character

Some models attach a cost to each of these operations

# Alignment
You can juxtapose two strings and insert spaces where required to visualise the difference between two strings
![String alignment diagram](https://ealizadeh.com/blog/tutorial-string2string/index_files/figure-html/fig-alignment-nw-plot-output-1.png)

## Notation
- $*$ and $\$$ represent different characters
- $-$ represents a gap
- There are 3 possible combinations:

| **s[i]:** 	| \* 	| - 	| \* 	| \* 	|
|-----------	|---	|---	|---	|---	|
| **t[j]:** 	| \* 	| \* 	| - 	| \$ 	|

# [[Dynamic Programming]] solution
- $d(i, j)$ is the distance between the $i^{th}$ prefix of $s$ and the $j^{th}$ prefix of $t$
    => Distance between $s$ and $t$ is $d(m, n)$
- The distance from an empty string to a string of length $k$ is $k$ because $k$ insertions are required

## Cases
For the cases in [[#Alignment#Notation|Alignment notation]]:
 \* + \*: recurse 
	- $d(i, j) = d(i - 1, j - 1)$

Otherwise: insert, delete, or substitute to find which path is most efficient
$d(i, j) = 1 + min\left( d(i, j - 1), d(i - 1, j), d(i - 1, j - 1)  \right)$
- - + \* insertion in $s$
- \* + -: deletion from $s$
- \* + \$: substitution into $s$

[[Recursion#Recurrence relation|Recurrence relation]]: 
$$
d(i, j) = \begin{cases} \\
	d(i, 0) = i & \text{base case}\\
	d(0, j) = j & \text{base case}\\
	d(i - 1, j - 1), & \text{if } s_i = t_j \\
	1 + min \left( d(i, j - 1), d(i - 1, j), d(i - 1, j - 1)  \right), & \text{if } s_i \ne t_j
\end{cases}
$$

## Properties
- [[Time Complexity]]: $O(mn)$
- Space complexity: $O(mn)$
	- Can be reduced to $O(m + n)$ if you only keep the most recent entry per column in the table
- Optimal alignment can be obtained via a [[#traceback]]

# Hand executing
![String distance table](https://vinayakgarg.files.wordpress.com/2012/12/editdist.png)

- Draw the following table with strings and red numbers
	- The red numbers represent the number of operations required if the strings were completely different
	- They are used when evaluating the $1 + \min(\dots)$ branch if there no evaluated cells nearby
- Go along the rows
	- Look at the left, top left, and top neighbours
	- Take the minimum and add 1
![[String Distance Table]]

## Traceback
![String distance table](https://vinayakgarg.files.wordpress.com/2012/12/editdist.png)

- The optimal alignment can be obtained using a traceback
- This entails tracing a path from the bottom right to the top left of the resultant table
	- Vertical steps are deletions
	- Horizontal steps are insertions
	- Diagonal steps are matches or substitutions
		- It was a match if the distance did not change
		- Otherwise, it must have been a substitution
- There are multiple optimal alignments --> there can be multiple optimal alignments (I.E paths of the same distance)