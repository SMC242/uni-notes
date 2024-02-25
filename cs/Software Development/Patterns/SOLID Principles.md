# Overview
A set of design principles for designing good object-oriented code. Applicable beyond OOP

- S: [[#Single responsibility]]
- O: [[#Open-closed]]
- L: [[#Liskov subsitution]]
- I: [[#Interface segregation]]
- D: [[#Dependency inversion]]

# Single responsibility
AKA SRP

- A class should do one thing and do it well
- Ideally, a class should have only one reason to change
- Anti-pattern: god objects
	- Objects that do loads of different things and therefore hold lots of state

# Open-closed
AKA OCP

- Classes should be open for *extension*, closed for *modification*
	- Extension: adding behaviour
		- Using the class for your purposes without affecting other consumers
	- Modification: changing behaviour
		- Could break assumptions other consumers have made
- [[Dependency Injection]] is often the solution
- Anti-pattern: fully closed classes
	- You have to edit the code to make it do new things
	- Symptom: overly restrictive, very few inputs

# Liskov substitution
AKA LSP

- If a class can't fully implement an interface, the interface needs to be split into smaller interfaces
- Code smell: classes raise errors saying "can't do this here"
- Anti-pattern: interfaces implemented by classes solving different concerns

# Interface segregation
AKA ISP

- Make small interfaces and favour [[Composition]] of interfaces
- Avoids having to implement things that are irrelevant to the class
- Anti-pattern: bloated interfaces
	- An interface that's doing multiple - potentially unrelated - things
	- You have to implement 4 things when you only want 2 of them

# Dependency inversion
- Depend on abstractions, not implementations
- This creates an abstraction layer between high-level interfaces and low-level functions
- Allows you to swap out the low-level implementation instead of being shoehorned into using one implementation
	- E.G one database driver
- Usually an interface is defined for the low-level functions to conform to
- Anti-pattern: direct dependencies
	- Depending directly on the low-level implementation 
	- You can't change low-level implementations