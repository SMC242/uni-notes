# Overview
Variables in a programming language are used to store a value and - for local variables - have a lifetime associated with them. A variable has a name and an associated value. The semantics of variables in a language depend on whether they are [[#Mutability|mutable]]

See also:
- [[Stack and Heap]]
- [[Pointers]]

# Mutability
- Variables are mutable if the value associated can be changed
- If they are not mutable, the variable's value can be inlined
- For mutable variables, getting the value means inspecting its current state

# Storage
- Variable stores are an array of cells
- A simple variable is contained in one cell
	- A [[Types#Primitive|primitive]] or pointer
- A composite variable is stored across a range of cells
	- Each [[Types|type]] will have its own cell structure

## Updating
- Updates are either total or selective (one component at a time)
- Updates are [[Functional Programming Terms#Side effect|side effects]]
	- Often not allowed in functional programming languages as they favour immutability

```c
struct Date {int y, m, d};

struct Date xmas, today;

// Selective updates
xmas.d = 25
xmas.m = 12;
xmas.y = 2008;

// Total update
today = xmas;
```

# Lifetimes
- The lifetime is the time between a variable's creation and destruction
- Global variables have lifetime = program 
- Local variables live inside their context (E.G a block or function)
- [[Stack and Heap#Heap|Heap-allocated]] variables live as long as they are allowed to 
	- Depends on the memory management system of the language (E.G manual, reference counting, garbage collection, ownership)
	- Accessed with pointers
	- An operation that allocates heap variables is called an "allocator"
		- `new` in Java
		- `malloc()` in C
	- Deallocators are implicit in automatic memory management systems
	- Deallocators are unsafe because they leave behind [[Pointers#Dangling pointers|dangling pointers]]
- Reachability refers to whether there is a live reference to a heap value

> [!NOTE] Overlapping vs Nesting
> - Two lifetimes overlap if they start or end at the same time
> - Two lifetimes are nested if one is contained within the other
>
> - Globals and locals never overlap
> 	- Globals are defined at the start of the program, which is earlier than the entrypoint - the earliest time a local can be initialised
> - Locals can overlap
