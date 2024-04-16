# Overview
Bindings are a name and their value (a [[Types#Primitive|value]], variable, or a function). These bindings are held in environments/namespaces

- [[#Binding types|Declarations]] may produce multiple bindings
	- `x = y = z = 1`
- Expressions and commands are evaluated within an environment

See also:
- [[Scopes]]
- [[Variables And Lifetimes]]

# Occurrences
- Binding occurrence: $I$ is bound to some entity $e$
	- A declaration
- Applied occurrence: when the $e$ that $I$ is bound to is used
	- Not necessarily via $I$ (E.G multiple references to the same $e$)

# Environment scoping
There are two main systems for deciding the environment used by a procedure

## Static
- When a procedure is executed in the environment it was defined in
- Decided at compile time
- Most languages fall into this category

## Dynamic
- When a procedure is executed in the environment where it is called
- Only known at runtime
- Used by lisps

# Binding types
## Declarations
- Constructs that lead to bindings
	- The process of creating the bindings is called "elaboration"
- Might have side effects
	- Creating a variable

## Definition
- A declaration with no side effects
- Only creates bindings

## Types of declarations
### Simple
- Type declaration
- Constant *definition*
- Variable declaration
- Procedure *definition*

### Compound
- Sequential declaration
	- Contains sub-declarations that depend on each other like a chain
- Recursive declaration
	- Depends on the bindings it creates