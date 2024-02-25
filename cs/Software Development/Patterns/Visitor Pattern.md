# Motivation
- You have many classes, each needing to implement a complex operation
- You don't want to modify those classes to add this operation
	- Perhaps to avoid violating the [[SOLID Principles#Single responsibility|single responsibility principle]]

# The pattern
- Create a class that manages holds all the implementations of an operation for the classes
- When given an object, the visitor knows which implementation to use ("visit")

## Examples
See the [Refactoring Guru article](https://refactoring.guru/design-patterns/visitor)