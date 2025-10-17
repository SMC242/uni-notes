---
tags:
---
# Overview
Polymorphism is a technique for reducing duplicated code in a programming language. Specifically, it allows you to operate with unspecified types in generic ways

# Parametric

- When you work with the shape of a parameter, not the concrete type
	- E.G you might desire a boxed type like `Functor f => f a`
- Type variables (E.G `a` and `b`) are used to show the shape of the parameters

> [!EXAMPLE]
> ```haskell
> f :: a -> b -> (a, b, a)
> f x y = (x, y, x)
> ```

## Calculating possible implementations

- `a -> b -> a` has 1 implementation
- `a -> b -> b` has 1 implementation ([[Properties|Commutative]] property)
- `a -> a -> (a, a)` has 4 implementations
	- Because `(a, a)` is a [[Types#Cartesian product|product type]]
- `Int -> a -> (Int, a)` has infinite implementations
	- Because there are infinite `a`s

See also: [[Types|Type theory]]

# Ad-hoc

- When you have the same function name but different implementations for each type
- Example: method overriding, Haskell type classes

> [!EXAMPLE]
> ```rust
> // A container that you can append elements to
> trait Append<T> {
> 	fn push(&mut self, x: T) -> bool;
> }
> 
> impl Append<u8> for ByteArray {
> 	fn push(&mut self, x: u8) -> bool {
> 		self.vec.push(x)
> 	}
> }
> 
> impl Append<bool> for ByteArray {
> 	fn push(&mut self, x: bool) -> bool {
> 		self.vec.push(u8:\:from(x))
> 	}
> }
> ```

# Monomorphisation

- When a compiler takes generic code and looks at which types it's actually used for, then generates duplicate definitions for each type
- More efficient because you don't need a [[Abstraction (PLs)#Virtual methods|V-table]]
- Employed by Rust and Go

> [!EXAMPLE] The previous example, monomorphised
> The code that calls each version will use the new names
> ```rust
> impl ByteArray {
> 	fn pushu8(&mut self, x: u8) -> bool {
> 		self.vec.push(x)
> 	}
> 	
> 	fn pushbool(&mut self, x: bool) -> bool {
> 		self.vec.push(u8:\:from(x))
> 	}
> }
> ```
# See also
- [[Abstraction (PLs)#Generics|How generics are implemented]]