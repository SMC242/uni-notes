# Overview
Searching for a substring (needle) in a string (haystack)
- Important for RegEx

> [!NOTE]
> ## Variants
> - Exact match
> - Approximate match
> - First occurrence
> - `n` occurrences

# Problem specification
- This note will focus on finding the first occurrence
## Variables
- $t$: the text
	- $n$: its length
- $s$: the pattern
	- $m$: its length
$n \gg m$

# Brute force strategy
- AKA exhaustive search because it tests all possible positions
- Average time complexity: $O(n)$
	- Most of the time, only one comparison is needed to know that there is a mismatch at that position
- Worst case time complexity: $O(mn)$
- 
## Algorithm
- Start at $position = 0$
- If the first character of $s$ is found at $position$, scan until all of $s$ is found or a character mismatches
	- Increment $position$ and repeat


# KMP
- On-line strategy
	- Doesn't backtrack
- Constructs a border table from $s$ before searching

## Border table
- An array $b$ where there is an element for each position $j$ in $s$
	- The same size as `s`
- A border of $s$ is a substring that is simultaneously a [[String Notation#Prefixes|prefix]] and a [[String Notation#Suffixes|suffix]], yet not equal to $s$
	- $Suffix(b, s) \land Prefix(b, s) \land b \ne s$
- Most strings don't have a border

> [!EXAMPLE]
> For the string `"a c a c g a t a c a c"`, the border table would be:
> - `a c`
> - `a c a c`

### Border table representation
| pattern (`s`) 	| a 	| b 	| a 	| b 	| a 	| c 	| a 	|
|---------------	|---	|---	|---	|---	|---	|---	|---	|
| j             	| 0 	| 1 	| 2 	| 3 	| 4 	| 5 	| 6 	|
| b[j]          	| 0 	| 0 	| 0 	| 1 	| 2 	| 3 	| 0 	|


## Algorithm
- If a mismatch is encountered, look in the border table to find out which character should be compared with the current character
- 