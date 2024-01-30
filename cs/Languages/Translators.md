# Overview
Translators convert code from one language to equivalent code in another language

- [[#Compiler]]
- [[#Interpreter]]
- [[#Transpiler]]

# Compiler
- Translates high-level code to low-level code
	- Source code to object code

> [!EXAMPLE]
> - JAVA --> JVM
> - C --> x86

## Assembler
- Translates assembly language to machine code

> [!EXAMPLE]
> - x86as --> x86

# Transpiler
- Translates between high-level languages

> [!EXAMPLE]
> - Java --> C
> - TypeScript --> JavaScript


## Disassembler
AKA decompiler

- Translates from low-level code to higher level code

> [!EXAMPLE]
> - JVM --> Java
> - x86 --> x86as

# Interpreter
- Accepts code in a language $S$ and immediately executes it
- No intermediate object code is generated
- Time-consuming unless the instructions are simple
	- Repeated analysis
	- Interpreting is slower than executing native machine code
	- This gets worse as the language gets more complex

## Process
One instruction at a time:
1. Fetch
2. Analyse
3. Execute

## Use cases
- Interactive evaluation of instructions (E.G terminal shell)
- Program is single-use
- Little duplication of instructions
- Simple instruction format
- Code has to be very portable

# Tombstone diagrams
![[Tombstone Diagram]]
# Virtual machines
![[Virtual Machines]]

# Compiling compilers
- If the compiler is written in the same language (E.G Java) introduces a chicken-and-egg problem
	- You can't run the interpreter until you have the compiler
- You can't run the compiler without an interpreter

Solution: [[Compiler Bootstrapping]]
