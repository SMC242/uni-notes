# Overview
The compilation stage where the following functions [[scopes|scope]] and [[Type Systems|type checking]] are performed. It takes in an [[Abstract Syntax Tree|AST]] and outputs an AST

# Type table
- A mapping of identifiers to their types
- Used for:
	- Checking if an identifier exists
		- Or is in scope
	- Checking for duplicate identifiers
	- Retrieving type information for an identifier

## Scope checking
- Table entries are tagged with their scope
	- E.G global, local, block
- Usually a separate table is created for each scope

# Type checking
- For each expression, check the type of its sub-expressions
	- From the sub-expressions, infer the type of the wider expression
	- Error if any unexpected types are found
- For each command, check the types of the expression within it
	- E.G for a C-style for loop, check that:
		- The first expression is an integer
		- The second expression is a boolean
		- The third expression is an integer

> [!NOTE] Terminology
> The process of traversing sub-expressions is called "[[Abstract Syntax Tree#Walking|walking]]". In OOP, this is implemented using the [[Visitor Pattern]]

