# Overview
Compilers 👍

# Phases
There are 3 phases of compilation:
- [[Syntactic analysis]]
	- Check that the program is well-formed (I.E checking [[syntax]])
	- [[#Lexing]], parsing
	- Outputs an [[Abstract Syntax Tree|AST]]
- Contextual analysis
	- [[Type Systems|Type checking]] expressions
	- Checking variable scopes and lifetimes
	- Outputs an [[Abstract Syntax Tree|AST]]
- Code generation
	- [[Translators|Translating]] to object code
	- Outputs object code

![Flow diagram of compilation phases](https://cdn1.byjus.com/wp-content/uploads/2022/03/phase-of-compiler.png)
