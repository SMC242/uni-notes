---
tags:
  - C
aliases:
  - Pointer
---
# Overview
Pointers are a reference to an [[Memory#Addresses|address]] in [[memory]]

# Getting a pointer
The address of a [[Pointers|pointer]] in C can be accessed with the following syntax:
```c
type * pointer = &pointer
```
where `type` is the type of the contents of the address

# Following a pointer
"Following" a pointer gets the value at the address

```c
type x = *pointer;
```

# Pointer arithmetic
- You can do basic arithmetic operations on a pointer
- These operations work in units of `sizeof(1 element)`
	- E.G for an array of `char`, `pointer + 1` will add 4 bytes

# Nested pointers
You can get a pointer to a pointer
```c
type x = ...;
type * ptr = &x;
type ** p_ptr = &ptr;
```

This is useful for making arrays of pointers
```c
char * [27] contents = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'x', 'y', 'z'};
```

# Pointer to local variable
Pointers to local variables should not be passed out of scope. This is because the lifetime of the variable may end before the pointer is de-referenced

> [!EXAMPLE]
> ```c
> char * get_a() {
> 	char a = 'a';
> 	return &a;  // Lifetime of `a` ends here
> }
>
>void print_a() {
>	char * a = get_a();
>	printf("%s", *a);  // `a`'s lifetime has already ended - `NULL` pointer
>}
>```

## Solutions
1. Pass in a pointer to the memory from the caller

```c
char * fill_a(* ptr) {
	*ptr = 'a';
}

void print_a() {
	char * a = malloc(1);
	fill_a(a);
	printf("%s", a);
	free(a);
}
```
2. Allocate some memory and pass the memory back to the caller

```c
char * get_a() {
	char * a = malloc(1);
	*a = 'a';
	return a;
}

void print_a(){
	char * a = get_a();
	printf("%s", a);
	free(a);
}
```