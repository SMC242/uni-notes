# Overview
Object Oriented Programming represents programs as a series of classes. Classes bundle data and functionality together. They may also [[#inherit]] from others

# Principles
## Encapsulation
- Restricting unneeded access to data
- Prevents people from messing with data that they shouldn't touch
- Pushes you to logically group functionality
- You can control what can be read and written via getter and setter methods

## Abstraction
- Hiding complexity from users
- Also specifying guidelines that should be followed such as [[#Interfaces]] and [[#Abstract base class]]es

## Inheritance
- Re-using code by taking it from parent classes
- Extending existing logic

See [[#Inheritance (concept)]]

## Polymorphism
- Methods doing different things depending on which class holds them
- This involves [[#overriding]] methods in child classes
- `move` might be implemented differently for a `Duck` vs a `Whale`, however a user can rely on there being a `move` method on all `Animals`. They don't need to care about the implementation

# Concepts
## Classes
Classes are a bundle of data ("attributes"/"members") and functionality ("methods"). They can be instantiated with specific values to create an [[#instance]]

They may also [[#Inheritance|inherit]] from other classes, meaning that they take the implementation of certain methods from their parent class

Additionally, they may implement an [[#Interfaces|interface]]

## Instances
Objects or instances are a class that has been applied to specific data. This might be a `Person` class that has been given `name = "Bob"`

## Inheritance (concept)
A class can use the implementations from another by inheriting from it. It may choose to [[#Overriding|override]] some or all methods, and will usually add new methods and members

A class that inherits from another is called a subclass or a child class. The class being inherited from is called a superclass or parent class.

## Overriding
Overriding is when a child class changes an implementation from its [[#Inheritance (concept)|parent class]]. It may still defer to the parent class sometimes or wrap the inherited behaviour with extra functionality ("extending")

## Abstract base class
An ABC can be inherited from, forcing you to provide implementations for the methods it specifies. ABCs provide no concrete implementations and cannot be [[#Instances|instantiated]]

## Interfaces
An interface is similar to an [[#Abstract base class]], but it is looser. It specifies what methods should exist and their type signatures

## Access modifiers
Members may be:
- Private: unable to be accessed from outside the class
- Public: able to read and written from outside
- Protected: can be read and written from inside the class and its children

# Inheritance vs interfaces
<ul class="breakdown">
	<li class="pro">Inheritance allows code re-use</li>
	<li class="con">In many OOP languages, a class can only have one parent</li>
	<br/>
	<li class="pro">Interfaces are like blueprints</li>
	<li class="pro">One class can implement many interfaces</li>
	<li class="con">No code is shared when using interfaces</li>
</ul>
