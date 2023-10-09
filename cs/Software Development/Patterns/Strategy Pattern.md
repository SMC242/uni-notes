A pattern used in [[OOP]]

# Motivation
- Inheritance struggles when requirements change
	- It can lead people to make silly mistakes like inheriting an incorrect protocol for communicating between banks
- Forcing someone to implement an interface instead of inheriting can lead to code duplication

# The pattern
- Create a class that represents a behaviour, then use it in other classes

## Example
All `Animal`s have a way to move, but they each move in different ways. Some fly, some swim, some walk

Create a class called `MovementManager` and create subclasses called `SwimmingManager`, `FlyingManager`, `WalkingManager`. Instances of `Animal` must provide a `MovementManager` instance