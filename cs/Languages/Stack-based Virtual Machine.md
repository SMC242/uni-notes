# Overview
In the context of [[Programming Languages Map|programming languages]], a type of [[Virtual Machines|virtual machine]] where variables are stored in a [[stack]]

> [!NOTE] Key concepts
> - The lifetime of a global variable = the entire run time
> 	- Stored in a fixed storage space
> 		- Note: in modern languages, these are stored in special segments of memory called the "code" and "data" segments. Stack-based VMs are
> 		- The code segment is for constants
> 		- The data segment is for mutable globals
> - Lifetimes of local variables can be nested
> 	- Local variables are pushed onto the [[Stack and Heap#Stack|stack]]

Advantages:
- [[Code Generation#Address allocation|Address allocation]] is easy
- [[Code Generation#Code selection|Code selection]] is easy
- [[Code Generation#Register allocation|Register allocation]] is not difficult

See also:
- [[Scopes]]
- [[Contextual Analysis]]
- [[Stack and Heap]]

# Activation frames
- There is always at least one frame at the base of the stack: the global variables
- For each active procedure, there is a frame containing its locals
	- The frame is pushed when the procedure is called
	- And popped when the procedure returns
- The size and layout of frames depends on the compiler
	- The size is picked at compile-time for each function
	- The layout is fixed for each function type (E.G variadic functions)
	- For each function, the compiler knows what the offset will be to get to the next frame
- Two pointers are maintained
	- Stack pointer: the first free cell in the stack (always above the top of the stack)
	- Frame pointer: the first cell of the topmost (current) frame

> [!NOTE] Active procedures
> "Active" procedures are procedures that are still running (I.E have been called but haven't returned yet)

Related: [[Stack and Heap#Call stack]] (the practical implementation of this)