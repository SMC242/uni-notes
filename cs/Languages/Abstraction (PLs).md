# Overview
This note focuses on abstraction in the context of programming language design

# Procedures
- Proper procedures (shorthand: procedure) are abstractions over a sequence of commands
	- Results in some [[Functional Programming Terms#Side effect|side effects]] like updating variables
- Function procedures (shorthand: function) are abstractions over expressions
	- Always returns a value
	- E.G a mathematical formula
- Procedures are said to be executed while functions are evaluated
- FP languages only have functions

## Function design
- In [[Functional Programming Map|Functional Programming]] languages, functions are designed as expressions
	- Simple
	- Translates mathematics easily
	- Less expressive
- In imperative languages, functions are designed as block commands (a sequence of commands until a `return` statement)
	- Fully expressive (can use commands)
	- Makes mathematical functions boilerplatey
	- Might not return a value if some branches lack a `return`
	- Side effects everywhere

## Parameters
- Arguments are values passed to procedures
	- $f(1)$ - 1 is an argument
- Actual parameters are expressions that are evaluated to get the argument
	- $f(g(x))$ - $g(x)$ is an actual parameter
- Formal parameters are identifiers for parameters
	- `def f(x):` - `x` is a formal parameter

### Parameter mechanisms
- Systems for retrieving arguments from formal parameters
- Either copy-based or reference-based

##### Copy parameter mechanisms
- [[Bindings|Binds]] a local [[Variables And Lifetimes|variable]] in the procedure
- When the procedure is called, a value is **copied-in**to the variable (the "copy-in"/"value" parameter)
	- When the argument is a value
	- Created and initialised when the procedure is called
	- Destroyed on return
- When the procedure returns, the value of the variable is **copied-out** of the variable (the "result" parameter)
	- When the argument is a variable (E.G from the enclosing [[Scopes|scope]])
	- A local variable is created on call for storing intermediate values  of the argument
		- This is how mutable arguments are implemented using pass-by-value
	- On return, the local's value is assigned to the argument variable (the one that was passed)
		- The local is destroyed after return
- Just because one is supported, doesn't mean the other is
	- E.G C only supports copy-in

#### Reference parameter mechanisms
- When the formal parameter is a reference to the argument (instead of the argument itself)
	- The formal parameter is used to *indirectly access* the argument
- Has a few types:
	- Constant parameter: the argument is a value
	- Variable parameter: the parameter is a variable
		- This means you can update variables in other [[scopes]]
	- Procedural parameter: the parameter is a procedure

# Packages
- Packages/modules are used to group components together under one name
	- Similar to [[Bindings|namespaces]] but not the same
		- One module may have multiple namespaces
- Can be used for encapsulation

# Classes
- Instance methods access members of an object
	- This object is called the *receiver object*

See also: [[OOP]]

## Virtual methods
- Virtual methods can be overridden
	- Some languages call this "final" or "frozen"
- Most OOP languages make methods virtual by default
- Dynamic dispatching is used when calling virtual methods
	- At compile time: the compiler figures out the [[Types|type]] of the object, then ensures that it has a method with a compatible name and siganture
	- At runtime: the receiver object's tag is used to find the actual class and see if an override is present

# Generics
- Packages, classes, types, and procedures that have a [[Types|type]] parameter
- A definition for each provided type is generated
	- E.G for `def f(x: T) -> T`, a definition would be generated for each type it is called with
		- `f(1)` generates `f(x: int) -> int`
		- `f("hello")` genereates `f(x: str) -> str`
- The typical `T1 extends T2` expression "bounds" the set of [[types]] that `T1` can be (`T1` is bounded by `T2`)