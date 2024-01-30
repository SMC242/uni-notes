# Overview
In the context of programming language [[Translators]], a virtual machine is a system that enables deploying code to many targets without explicitly compiling for them. This is in contrast to a real machine, which directly executes machine code

## Motivation
- Instruction sets and APIs vary between:
	- [[CPU]] architectures
	- [[Operating System]]s
	- Environments
- Compiling to all possible targets is not feasible

# Definition
AKA abstract machine

- A machine where the machine code (often actually bytecode) is executed by an interpreter
- Used for portability
	1. Compile the program down to bytecode
	2. Interpret the bytecode on the user's machine, executing the correct instructions for their platform

# Portability
> A program is portable if it can be made to run on different machines with minimal (or no) change

- A compiler that generates native machine code isn't portable because it has to be set to target different machines
	- You would need to compile to all possible targets (unfeasible)
- A compiler that generates VM code can be portable
	- The interpreter will generate the platform-specific instructions when the VM code is executed

# Interpretive compilers
Problem:
- [[Translators#Compiler|Compilers]] take a while to compile, but the resulting code runs fast
- [[Translators#Interpreter|Interpreters]] execute sooner, but execute slowly

Solution:
- Translate the source code to VM code and interpret it later
- Fast translation
- Medium execution speed

## VM code
- An intermediate-level language
	- Lower level than the source code, higher level than native machine code
- Translation from source code to VM code is fast
- VM instructions are simple, so an interpreter can analyse them quickly

> [!NOTE] Java
> - Java compiles to JVM code
> - JVM provides instructions for various purposes
> 	- Object construction
> 	- Method calls
> 	- Array indexing
> - JVM instructions are calle bytecodes
> 	- Similar format to native machine code
> 		- opcode and operand

# Just-in-time compiler
- JIT compilers translate VM code to native machine code right before execution

## Selectivity
- In Java, the JIT compiler only compiles frequently-called ("hot") methods
- The interpreter counts method calls
- The remaining instructions are interpreted

