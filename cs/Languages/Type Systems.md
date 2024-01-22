---
tags:
  - TypeTheory
---
# Overview
- Type systems exist to avoid type errors by grouping values into [[types]]
- Type errors are when a program performs an undefined operation

> [!info] High-level vs low-level languages
> High-level languages have some form of type system. Low-level languages only have words and bytes

# Type checking
- Before an operation runs, the operands must be type-checked to ensure that the operation is defined for the operands
- [[#Static]] type systems choose to do this at compile time
- [[#Dynamic]] type systems do this at runtime

## Static
Characteristics of a statically-typed programming language:
- Every variable has a fixed type that does not change
- Every expression has a fixed type
	- Typically inferred by the compiler
- Operands are type-checked at compile-time

## Dynamic
Characteristics of a dynamically-typed programming language:
- Only values have types
- Variables don't have fixed types
- Expressions don't have fixed types
- Operands are type-checked at runtime before the operation runs

> [!WARNING] Efficiency
> Having to type-check before all operations adds overhead. Additionally, all values have to be tagged, leading to increased memory consumption

