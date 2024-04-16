# Overview
Scope refers to the region of code where a variable is defined

# Scoping systems
- A language might use multiple scoping systems
	- It's common to have [[#flat block]]-scoped functions and [[#nested]] variable scopes

## Monolithic
- There is only one scope

## Flat block
- When there is only a global and local scope

```c
int b = 1;

int func() {
	// b is defined here
	int c = 2;
}
// c is not defined here, but b is
```

## Nested
- When variables can be contained within a block

```c
int func() {
	int x = 2;
	
	{
		// x is defined here
		int y = 3;
	}
	// y is not defined here, but x is
}
// x is not defined here
```