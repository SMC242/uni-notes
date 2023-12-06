# Overview
There are various ways to model a computer

# Finite-state automata
![[Finite-state Automata]]


# Pushdown automata
![[Pushdown Automata]]

# Turing machines
![[Turing Machines]]

# Counter machines
A programming language condensed into its simplest possible form

## Features
- `int` datatype
- Lines are terminated by a semicolon `;`
- Labels
	- Of the form `label: statement`
- Statements:
	- Assignment: `x = 0`
	- Addition of a constant: $x = y + 1$
	- Subtraction of a constant: $x = y - 1$
	- Conditional goto: `if x == 0 goto label`
	- `halt`

# Church-Turing thesis
[[Turing Machines]] can be used to compute anything that is "effectively computable"
- There is a program that computes the value of any function using all computers and programming languages humanity is currently aware and unaware of
 
 A variety of computational models can be used to compute any "effectively computable" function: 
 - [[Lambda Calculus]]
 - [[Turing Machines]]
 - [[Recursive Functions]]
 - Productions systems
 - [[#Counter machines]] and general purpose programming languages