# Overview
- In order to avoid writing compilers in machine code, compilers are compiled in a long chain
- Bootstrapping is the process of compiling a compiler with compilers of increasing complexity
	- Eventually, you end up compiling the compiler with itself

See first: [[Tombstone Diagram]]

# Chick-egg problem
- A compiler has to be compiled

> [!question] How did we get here?
> Problem: how do we get a C compiler implemented in machine code? First, you need an assembler implemented in machine code. How did you get that? You wrote it in machine code
> 
![[c-compiler.excalidraw]]

# Improving compilers
- Writing your compiler in assembly isn't realistic for a complex language
- Solution: write it in a high level language

> [!QUESTION] But how?
> Compile your new C-based compiler with the old one
>
>![[bootstrapping.excalidraw]]

## Upgrading compiler binary
Once you have the compiler implemented in C, you can compile it with itself

![[migrating-compilers.excalidraw]]

> [!QUESTION] Why?
> The old assembly-based compiler might produce binaries with poor performance