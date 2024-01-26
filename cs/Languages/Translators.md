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

# Tombstone diagram
- Used to visually represent translations between languages

![Symbols](https://3.bp.blogspot.com/-MRW-nPtm9no/UuKKgMceHJI/AAAAAAAAAKU/zHH3o0Dz_5M/s1600/post6.pic1.png)

> [!EXAMPLE] Java
> ![Java tombstone diagram](https://media.geeksforgeeks.org/wp-content/uploads/20211009220451/img10.JPG)

## Runtime
This diagram represents a runtime

![Machine compatability](https://media.geeksforgeeks.org/wp-content/uploads/20211009215724/img9-300x211.JPG)

> [!NOTE]
> In this case, $M$ denotes a machine and its corresponding machine code

## Compiler
This is a tombstone diagram for a compiler

![Compiler diagram](https://www.researchgate.net/publication/269292713/figure/fig3/AS:669349111164958@1536596672662/An-introduction-to-Tombstone-diagrams.png)

# Machines
## Real machine
- A machine where machine code is executed by hardware

## Virtual machine
AKA abstract machine

- A machine where the machine code (often actually bytecode) is executed by an interpreter
- Used for portability
	1. Compile the program down to bytecode
	2. Interpret the bytecode on the user's machine, executing the correct instructions for their platform

# Interpretive compilers
Problem:
- Compilers take a while to compile, but the resulting code runs fast
- Interpreters execute sooner, but execute slowly

Solution:
- Translate the source code to [[#virtual machine]] code and interpret it later
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

## Just-in-time compiler
- JIT compilers translate VM code to native machine code right before execution

### Selectivity
- In Java, the JIT compiler only compiles frequently-called ("hot") methods
- The interpreter counts method calls
- The remaining instructions are interpreted

## Portability
> A program is portable if it can be made to run on different machines with minimal (or no) change

- A compiler that generates native machine code isn't portable because it has to be set to target different machines
	- You would need to compile to all possible targets (unfeasible)
- A compiler that generates VM code can be portable
	- The interpreter will generate the platform-specific instructions when the VM code is executed

### Development kits
- If the compiler is written in the same language (E.G Java) introduces a chicken-and-egg problem
	- You can't run the interpreter until you have the compiler
- You can't run the compiler without an interpreter

Solution: 
