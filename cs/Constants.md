---
tags:
  - C
---
# Overview
Constants are a type of variable that cannot be modified. They appear in many languages, such as [JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/const), [Java](https://www.geeksforgeeks.org/final-keyword-in-java/) (kind of), and [C](https://www.geeksforgeeks.org/constants-in-c/)

# Syntax
```c
const type name[ = ...];
```

There are three types of constants in C:
1. Constant addresses
The address of the [[pointer]] can't change
```c
const type * name;
```

2. Constant values
The value that the [[Pointers|pointer]] points to cannot change
```c
type * const name;
```

3. Both
Neither the address nor the value can change
```c
const type * const name;
```