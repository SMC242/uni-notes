# Overview
How the compiler selects architecture-specific instructions to output. This problem is abstracted away with [[Virtual Machines]], but someone has to write them

> [!info] Key Information
> - Reduced Instruction Set Computer (RISC) is a common CPU architecture used by AMD and in phones (ARM)
> - Complex Instruction Set Computer (CISC) is another common architecture used by Intel processors
> 	- Generally being phased out

> [!WARNING] Challenges
> - Instruction sets are large and complicated
> 	- There are multiple instruction sets in use
> - Supporting both RISC and CISC systems is difficult due to differing register conventions
> 	- RISC only has general-purpose registers
> 	- CISC has special-purpose registers

See first:
- [[Code Generation]]

# Register allocation
- Programs that use registers more are faster --> aim to work in the registers as much as you can
- Limited number of registers
- Strategy: determine when variables are "live" and remove them from the registers when they are not
	- A variable is considered live if it could be inspected in the future


## Basic blocks
- Basic blocks are a sequence of instructions with little jumping
	- Only jumping at the end or to the start of the the block
- Given some code that meets these requirements, you can split its sub-expressions into temporary, intermediate variables
	- This form is called "three-address-code"
		- It involves 1 assignment and two operands involved in a binary operation
- These variables can then be assigned to registers

> [!EXAMPLE]
> This line of C should be broken down
> ```c
> a = (b + c) *  (d - e);
> ```
> into this
> ```
> t1 <- b + c
> t2 <- d - e
> a <- t1 * t2
> ```

## Control-flow graph
- To handle jumps, build a control-flow graph
	- A [[Directed Graph]] of basic blocks
- Variables are considered live until no more branches of the graph use them

![Example control-flow graph](https://www.researchgate.net/publication/4065402/figure/fig5/AS:668826706395151@1536472121671/Example-of-Control-Flow-Graph.png)

## Liveness analysis
- For each basic block in the control-flow graph, the following sets are created:
	- $in_b$: variables live at the start of $b$
	- $out_b$: variables live at the end of $b$
	- $use_b$: variables inspected by $b$
		- Before the variables are updated
	- $def_b$: variables updated (or *defined*)  by $b$
		- Before the variables are inspected
- $in_{b} =  use_{b} \cup (out_{b} - def_{b})$
- $out_{b} = in_{b'} \cup in_{b''} \cup \dots$
	- $b'$, $b''$ are successors for $b$


# Intermediate representation
- An intermediate form that has both the semantics of the source code and the target machine's instructions
- Usually independent of the target machine
- A tree similar to an [[Abstract Syntax Tree|AST]] is generated when translating to the IR
	- Called the "IR tree"
- It will have some patterns that represent ways of doing certain operations
	- Think of them as [[Code Generation#Templates and actions|code templates]]

## Code selection
1. Translate the [[Abstract Syntax Tree|AST]] to an IR tree
2. Cover the tree with IR instruction *patterns*
	1. There are multiple covers for each construct (I.E different ways to compute the same thing)
3. Emit the corresponding code for the instruction patterns
	1. Allocate registers as you go

### Maximal munch
- An algorithm for code selection
- [[Time Complexity]]: $O(t)$ where $t$ is the IR tree
- Optimal in two ways:
	- No two adjacent patterns can be replaced with one pattern
		- I.E always picks the largest pattern
	- Minimises the number of instructions
- Doesn't necessarily produce the most performant code

For an IR tree $t$ and using instruction patterns $ps$:
1. Find the biggest pattern $p$ that covers the top of $t$
2. For each sub-tree that wasn't covered (left-to-right), cover and emit the instructions for $p$