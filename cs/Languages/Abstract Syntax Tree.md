---
aliases: AST
---
# Overview
AKA AST

A [[maths/Trees/Tree|Tree]] that represents the source program after [[Compilation Phases#Phases|syntactic analysis]]

See first: [[Syntax]]

![AST example](https://ruslanspivak.com/lsbasi-part7/lsbasi_part7_ast_02.png)

# Structure
- Each [[maths/Trees/Tree#Nodes|leaf node]] is an identifier or literal
- Each internal node is a construct
	- E.G commands or declarations
- Much more compact than [[Syntax#Syntax trees|syntax trees]]

# Walking
- "Walking an AST" refers to traversing a tree and its sub-trees, doing something as you go