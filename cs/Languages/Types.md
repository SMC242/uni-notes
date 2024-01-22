---
tags: TypeTheory 
---
# Overview
Programming languages use various types to store data. Some are [[Type Systems#Dynamic|dynamically typed]], others are [[Type Systems#Static|statically typed]], but both use types. Type theory is founded in [[Maths Map#Set theory|Set Theory]]

# Definition
Values can be grouped into types based on what operations are defined for them

> A type is a set of values with operations that are defined for every value

# Notation
Types are written in UPPERCASE and values are written in lowercase

## Cardinality
The [[Set Operations#Cardinality|cardinality]] of a type is denoted by `#T` where `T` is the name of a type

- `#Bool`
- `#Int`
- `#List`

# Primitive
- A type whose values can't be decomposed into others
- Primitives are the fundamental types of a programming language

Typical built-in primitives include:
- `VOID = {void}`
- `BOOL = {false, true}`
- `CHAR = {"A", ..., "Z", "0", ..., 9, ...}`
	- Some languages support Unicode
- `INT = {-m, ..., -2, -1, 0, +1, +2, ..., m - 1}`
	- Where `m` is the maximum/minimum integer supported by the language or implementation
- `FLOAT = {...}`
	- Language/implementation defined

> [!NOTE] Names have different meanings
> A BOOL in one language can mean a single bit, while in another it could mean a small integer



# Composite types
A type made up of another types

## Cartesian product
AKA product types

- Used for tuples, structs, and records
- Where values of 2 or more types are grouped into pairs/tuples
- It is defined by taking the [[Set Operations#Cartesian product|Cartesian product]] of the types
- Pascal records, C structs, Haskell tuples are Cartesian product types
	- Python tuples are not because they aren't mathematical tuples
	- Tuples can't be indexed

Cardinality: $\# (S \times T) = \# S \times \# T$
- Note that $\times$ in the left hand side is the arithmetic multiplication operator and the $\times$ on the right hand side is the Cartesian product operator 

> [!NOTE] Pairs and tuples...
> Pairs can be generalised as tuples because they are a special case of tuples

### Operations
- Construction of a tuple 
- Selection of a component from a tuple

> [!EXAMPLE] C Example
>```c
>enum Month {
>	JAN, FEB, MAR, ..., DEC
>};
>struct Date {Month m; int d;};
>
> struct Date date1 = {JAN, 1};  // Struct construction
> printf("%d/%d", date1.d, date1.m + 1);  // Component selection
> ```
> 
> The values of `Date` are $DATE = MONTH \times INT$

## Disjoint union types
AKA sum types

- Used for algebraic types (enums), variant records, objects
- When a value is chosen from two or more *different* types
- Represented by $S +T$
	- Each value is called a variant and each variant is given a tag
	- $S + T = \{ left \ x | x \in S \} \cup \{ right \  y | y \in T \}$
	- $left$ and $right$ are tags in this case
	- Tags are just names for the value
- Haskell's ADTs, Pascal/Ada variant records, and Java [[#objects]] are all disjoint unions. Most enums are too

Cardinality: $\# (S + T) = \#S + \#T$
	- This is why they are called sum types

> [!EXAMPLE]
> ```typescript
> // Each of these tags will have values (integers) assigned by the compiler
> enum Colours {
> 	RED,
> 	GREEN,
> 	BLUE
> }
>
> // "success" and "error" become the tags
> type Response = {type: "success"; data: string} | {type: "error"; errorCode: int};
> ```

### Operations
$T_{1} + T_{2} + \dots + T_{n}$
- Construction from a tag and its variant
- Tag test: checking the tag of a value
- Projection: getting the specific variant of a value

> [!EXAMPLE] Java example
> ```java
> // Construction
> Circle c = new Circle(5);
> Box b = ew Box(3, 4);
>
> Point p = false ? b : c;  // Could be a Box or a Circle
> // Tag test
> if (p instanceof Circle) {
> 	int radius = ((Circle)p).r;  // Projection
> }
> ```

### Objects
- The type of all possible objects in a program can be represented with a disjoint union type
- Abstract classes are excluded because they can't be constructed

>[!EXAMPLE]
>Given the types `Point`, `Circle(INT r)`, and `Box(INT w, INT h)`, the type of an object in the generated program would be:
>```
>OBJECT = 
>	+ Point VOID
>	+ Circle INT
>	+ Box (INT x INT)
>	+ ...  // Other classes
> ```

## Mappings
- Used for arrays and functions
- A mapping from one type to another  $m : S \rightarrow T$
	- If $m$ maps a value $x$ to value $y$, $y = m(x)$ and $y$ is known as the *image* of $x$ under $m$
- $S \rightarrow T$ is the type of mappings from $S$ to $T$ (I.E functions)
	- $S \rightarrow T = \{ m | x \in S \Rightarrow m(x) \in T \}$

Cardinality:
$\# (S \rightarrow T) = (\#T)^{\# S}$

See also: [[Functions|Mathematical functions]]

### Arrays
![[Array Types]]

### Functions
![[Function Types]]

## Recursive types
- Used for lists and trees
- A disjoint union defined in terms of itself
	- Has at least one recursive case and non-recursive case

Cardinality: $\#T = \infty$

> [!EXAMPLE]
> - $LIST = VOID + (T \times LIST)$
> - $TREE = VOID + (VALUE \times TREE \times TREE)$

### Lists
![[List Types]]

# Expressions
Expressions also have types

> An expression $E$ is of type $T$ if, when evaluation of $E$ terminates without failure, is guaranteed to yield a value $v$ of type $T$

> [!EXAMPLE] Java expressions
> - `n - 1` is of type INT
> - `n > 0` is of type BOOL

There are two categories of expressions:
- Simple
	- Literals
	- Variables
- Compound
	- Function calls
	- Constructions
	- Conditional expressions
	- Iterative expressions
	- Block expressions (contains local variables)