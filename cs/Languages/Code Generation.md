# Overview
The [[Compilation Phases|compilation stage]] where an [[Abstract Syntax Tree|AST]] is transformed into object code

3 main sub-stages:
1. [[#Address allocation]]
2. [[#Code selection]]
3. [[#Register allocation]]

- Implemented in OOP languages with the [[Visitor Pattern]]

See also:
- [[Native Code Generation]]

# Address allocation
- Deciding how and where variables will be stored in memory
- Collecting and communicating information about what variables and procedures exist
- An address table is maintained 
	- A mapping of identifiers to their addresses

> [!EXAMPLE] Allocating Variables vs Procedures
> - Allocating variables: allocate address in memory, store it
> - Allocating procedures: store the address of its entry-point

See also: [[Memory]]
## Deallocation
- When the variable reaches the end of its lifetime, it has to be removed from memory
- Programming languages define lifetimes in many different ways
	- Manual memory management (C)
	- Block scoping
	- Reference counting
	- Rust's lifetimes

See:
- [[Stack-based Virtual Machine]]s

## Address types
- Addresses are tagged with a type indicating the region of memory they are stored in
- There are 3 types ("locales"):
	- Code: an instruction within the object code's memory space
	- Global: an location within the global variable region
	- Local: a location within a group of local variables
# Code selection
- Generating the correct code for the given constructs
- Code templates are employed
	- The object code that will be generated for a given construct

> [!EXAMPLE] Templates For Expressions vs Commands
> Some extra code needs to be included on top of the object code for the construct
> 
> - Expressions: include code to evaluate sub-expressions
> - Commands: include code to evaluate sub-expressions and sub-commands

## Templates and actions
- Templates decide what object code is selected
- Actions are how the code generator generates the selected code
- The template is selected, then a series of actions run

> [!EXAMPLE] Example Expression
>
> ```mermaid
> ---
> title: AST
> ---
> flowchart TD
> 	id1[PLUS] --> id2[EXPR1]
> 	id1[PLUS] --> id3[EXPR2]
> ```
>
>Template:
>- Evaluate `expr1`
>- Evaluate `expr2`
>- `ADD`
>
>Action:
>1. [[Abstract Syntax Tree#Walking|Walk]] `expr1`, generate code
>2. [[Abstract Syntax Tree#Walking|Walk]] `expr2`, generate code
>3. Emit instruction: `ADD`

> [!EXAMPLE] Example Command
> ```mermaid
> ---
> title: AST
> ---
> flowchart TD
> 	id1[ASSN]
> 	id2[ID\n'x']
> 	id3[expr]
> 
> 	id1 --> id2
> 	id1 --> id3
>```
>
>Templates:
>where `d` is an address offset of the address for the symbol `x`:
>- `STOREG d` (global variable)
>- `STOREL d` (local variable)
>
>Actions:
>1. Walk `expr`, generate code
>2. Look up `x` in the address table to get address `d`
>3. Check the [[Scopes|scope]] of the assignment
>	1. Global? Emit `STOREG`
>	2. Local? Emit `STOREL`

## Jumps
- Instructions are emitted one-by-one
- Problem: if a jump goes to unvisited code, the code generator doesn't know what the destination address is
- Solution: go back and patch the address in when it's found
	- The instruction's address is initially set to 0 (placeholder)

> [!EXAMPLE] Generator For If Commands
> ```mermaid
> ---
> title: AST
> ---
> flowchart TD
> 	id1[IF]
> 	id2[expr]
> 	id3[com]
> 
> 	id1 --> id2
> 	id1 --> id3
>```
>
>Actions:
>1. Walk `expr`, generate code
>2. Emit `JUMPF 0`
>3. Walk `com`, generate code
>4. Patch address into `JUMPF`


# Register allocation
- Assigning registers for local or temporary variables
- Not always a feature of a language
