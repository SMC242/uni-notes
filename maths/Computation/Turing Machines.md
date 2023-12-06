# Overview
They have these properties:
- Simple
- Unlimited memory
	- Memory can be used arbitrarily
- Almost as powerful as a modern computer

# Definition
- $T$: a Turing Machine
- $\Sigma$: a finite alphabet of symbols
	- $\#$ is the empty symbol
- An unbounded tape of squares
	- Each square holds a symbol
	- Unbounded in both directions
- A tape head
	- Can read and write squares
	- Can move left or right along the tape
- $S$: the set of states
	- $S_0$: the start state
	- $S_{y}, S_{n}$: halt states
- A transition function (program)
	- $f: ((S / \{S_{y}, S\}) \times \Sigma) \rightarrow (S \times \Sigma \times \{Left, Right \})$
	- E.G $f(s, \sigma) = (s', \sigma', d)$

> [!INFO] Reminder
> \\ is the [[Set Operations#Difference|set difference]] operator 

# Transition functions
Transition functions have two components:
- $f(s, \sigma)$ where $s \in S, \sigma \in \Sigma$
	- If in state $s$, read $\sigma$ from the tape
- $= (s', \sigma', d)$ where $s' \in S, \sigma \in \Sigma, d \in \{Left, Right \}$
	- Transition to $s'$
	- Overwrite $\sigma$ with $\sigma'$
	- $d$ dictates whether the tape head moves left or right

## Transition relations
In the case of a non-deterministic Turing Machine, the transition function would be replaced with a transition relation

$$f \subseteq ((S \times \Sigma) \times (S \times \Sigma \times \{ Left, Right \}))$$

A transition relation has multiple possible outputs for a given state and symbol

# Running
- The input string is written to the tape
- The tape head is placed on the leftmost (first) symbol
- The program runs until it reaches a termination state
	- $S_{y}$ represents acceptance (a "yes" answer)
	- $S_n$ represents rejecetion (a "no" answer)

# Writing algorithms
- Writing a Turing Machine is prohibitively difficult
- Write pseudocode instead

Use this set of instructions:
- `read`
- `erase`
- `move`
- Branching: `if`, `else`, `else if`
- `halt`

> [!EXAMPLE] Palindrome pseudocode
> ```
> read symbol
> erase symbol, enter a state that remembers it
> move to end of input
> if only blank symbols remain do
> 	enter accepting state, halt
> else if last symbol = last erased symbol do
> 	erase symbol
> else
> 	enter rejecting state, halt
>
> if end of input do
> 	enter accepting state, halt
> else
> 	move to start of unprocessed input
> 	go to step 1
> ```

## Translation
Pseudocode can be translated to a formal definition

> [!EXAMPLE] Translating the palindrome pseudocode
> ### Transitions
> - From $s_0$: enter $s_y$ if blank is read, otherwise:
> 	- Move to $s_1$ if an "a" is read
> 	- Move to $s_2$ if a "b" is read
> 	- Regardless: erase the symbol
> - Remain in $s_1$ or $s_2$, moving right until a blank. Upon reading a blank, enter $s_3$ or $s_4$ and move left
> - From $s_3$ or $s_4$, enter $s_y$ if a blank is read, $s_n$ if an unrecognised symbol is read. Otherwise: erase, enter $s_5$, and move left
> - In $s_5$, move left until a blank is read. Then move right and enter $s_0$
>
> ### States
> $s_0$: reading, erasing, and remembering the leftmost symbol
> $s_{1}, s_2$: moving right until the end, remembering the erased symbol
> $s_{3}, s_4$: checking for the expected rightmost symbol
> $s_5$: returning to the leftmost symbol

# Transition diagrams
State transition diagrams are [[directed graph]]s where:
- Each state is a vertex
- $f(s, \sigma) = (s', \sigma', d)$ is an edge between $s$ and $s'$ 
	- The edge is labelled $(\sigma \rightarrow \sigma', d)$
		- $\sigma \rightarrow \sigma'$ means $\sigma$ is overwritten by $\sigma'$
		- $d$ is the direction the tape head will move

> [!EXAMPLE] 
> ![Example](https://media.cheggcdn.com/media/7e7/7e7ae1dd-7238-4ea1-a2c4-56ac58412de1/phpy37Gip.png)

# Languages
A Turing Machine can output more than yes or no. Its output could be whatever remains on the tape. This means that $S_y$ and $S_n$ are not used

Formally:
- $L$: the language
- $f(x) = y$ where $x$ is the initial string and $y$ is the string string after halting
	- E.G $f(x) = 1\ if\ x \in L,\ otherwise\ 0$

> [!EXAMPLe] Program for adding 1
> ```
> move right until symbol = #
> move left until the symbol = 0 or #
> if 0 or blank found do
> 	write 1
> 	move right until # and do
> 		if symbol = 1 do
> 			write 0
> 	halt
> ```

# Turing-computable
A function is Turing-computable if there could be a Turing Machine that halts for any input $x$

> For any input $x$, the machine $T$ halts with output $f(x)$

## Recognisable vs decidable
- A language is decidable if a Turing Machine exists that would halt in either $S_y$ or $S_n$
- A language is recognisable if a Turing Machine exists that would either:
	- Halt in $S_y$ or $S_n$
	- In the case of a "no" instance, never halt
- Recognisable problems are a superset of decidable problems
	- The [[Halting Problem]] is Turing-recognisable but not Turing-decidable because it would never halt if given a "no" instance

### Formally...
A language $L$ is Turing-recognisable if a Turing Machine could recognise it. I.E:
- If $x \in L$ then $T$ halts in $S_y$
- If $x \notin L$ then $T$ halts in $S_n$ *or* does not halt

A language is Turing-decidable if a Turing Machine could decide it. I.E:
- If $x \in L$ then $T$ halts in $S_y$
- If $x \notin L$ then $T$ halts in $S_n$

# Enhancements
- Multiple tapes
	- I.E multiple input/output streams
- 2d tapes
	- Taking in a matrix instead of a vector
- Non-determinism
	- The transition function could have multiple possible outputs
	- This does not increase the computing power

A normal Turing Machine can simulate any enhanced Turing Machine