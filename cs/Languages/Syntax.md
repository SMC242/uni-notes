# Overview
A programming language's syntax specifies how expressions, commands, and declarations can be arranged in a valid program

See also:
- [Article on operator priority and precedence](https://medium.com/@mbednarski/operator-priority-and-associativity-in-ebnf-grammar-3a9f23dd9daf)

# Specifications
Syntaxes can be specified either formally or informally

## Informal
A specification in natural language such as:

> A function not wrapped in the `IO` monad cannot have side effects
## Formal specifications
A specification using a mathematical notation
- Pro: More precise
- Pro: typically more concise
- Pro: reduced ambiguity
- Con: Accessibility. You need to know the notation
- Con: Hard for a complex programming language

# Specification notations

## Regular Expressions
AKA RegEx or REs

- Good for lexical elements
	- Identifiers
	- Literals
	- Comments

> [!EXAMPLE] Examples
> - $xyz$ matches "xyz" exactly
> - $RE_{1} | RE_{2}$ matches either $RE_{1}$ or $RE_{2}$
> - $RE_{1}RE_{2}$ matches $RE_{1}$ concatenated with $RE_{2}$
> - $RE^{*}$ matches 0 or more $RE$
> - Parentheses are used for grouping: $(RE)$

> [!NOTE] Extended RE notation
> These constructs may or may not be provided by a RegEx engine:
>
> - $RE^{+}$ one or more $RE$
> 	- Equal to $RERE$
> - $RE^{?}$ 0 or 1 $RE$
> 	- Equal to $RE|$

> [!NOTE] Unix RegEx
> Unix shells use ad hoc pattern-matching. It doesn't have $RE_{1} | RE_{2}$ and $RE^{*}$
> 
>- $[...]$ matches any one of the characters between the brackets
>	- $[ab] = a|b$ 
>- $?$ on its own matches a character
>- $*$ matches a string of 0 or more characters
>
> `egrep` provides the missing features of Unix RegEx and:
> - $.$ matches a single character

## Backus Naur Form
AKA BNF

- Good for larger and/or nested constructs
	- Expressions
	- Commands
	- Declarations

> [!NOTE] Syntax
> $N = \alpha | \beta | ... | \gamma$ where $\alpha, \beta, \gamma$ are terminal or non-terminal symbols

> [!EXAMPLE]
> ```EBNF
> prog = eof 
> 	| com prog
> 	
> com = "put" expr eol
> 	| "set" id "=" expr eol
> 	
> expr = prim
> 	| expr "+" prim
> 	| expr "-" prim
> 	| expr "*" prim
>
>prim = num
>	| id
>	| "(" expr ")"
>	
>num = digit | num digit
>id = letter
>letter = "a" | "b" | ... | "z"
>digit = "0" | "1" | ... | "9"
>eol = "\n"
>```

## Extended Backus Naur Form
AKA EBNF

- [[#Backus Naur Form#BNF]] with [[#Regular Expressions|REs]]

> [!EXAMPLE]
> ```EBNF
> prog = com * eof
> 
> com = ‘put’ expr eol
> 	| ‘set’ id ‘=’ expr eol
> 	
> expr = prim ( ‘+’ prim | ‘-’ prim | ‘*’ prim )*
> 
> prim = num
> 	| id
> 	| ‘(’ expr ‘)’
> 	
> id = ‘a’ | ‘b’ | ‘c’ | … | ‘z’
> num = (‘0’ | ‘1’ | … | ‘9’)+
> eol = ‘\n’
> ```

# Grammar

The set of rules of a language that define how the phrases in the language can be formed

> [!NOTE] Symbols
> There are two types of symbols:
>
>## Terminal symbols
>A lexical element such as "const"
>
>## Non-terminal symbols
>A symbol that refers to a phrase that is part of a sentence such as `expr`
>
>### Sentence
>The non-terminal symbol that forms a complete sentence

## Formal definition
- Terminal nodes must be visited from left to right
- If $S$ is the sentence symbol of the grammar $G$, a sentence of $G$ is of class $S$
- A language created by a grammar $G$ is the set of all sentences in $G$
## Production rules
These rules define the possible values of a non-terminal symbol and how they can be composed 

> [!EXAMPLE]
> ```EBNF
> sentence = subject verb object "."
> subject = "I" | "a" noun | "the" noun
> object = "me" | "a" noun | "the" noun
> noun = "cat" | "mat" | "rat"
> verb = "see" | "sees" | "smell" | "smells"
> ```
>
>Example `sentence` using these production rules: "A cat smells the rat"

## Syntax trees
A [[maths/Trees/Tree|Tree]] representing a particular phrase in the language. Leaves are terminal symbols and non-leaf nodes are non-terminal symbols

> [!EXAMPLE]
> Here is the syntax tree for the following program:
> ```
> while b ≠ 0:
>     if a > b:
>         a := a - b
>     else:
>         b := b - a
> return a
>```
> ![Syntax tree](https://upload.wikimedia.org/wikipedia/commons/thumb/c/c7/Abstract_syntax_tree_for_Euclidean_algorithm.svg/1200px-Abstract_syntax_tree_for_Euclidean_algorithm.svg.png)

## Associativity
Following this [[#Ambiguity|ambiguous]] example: $a + b + c$

- Operators can either be left or right [[Associative Property|associative]]
	- This is the operand where the brackets will be placed if there are nested operations
	- Left associative: $(a + b) + c$
	- Right associative: $a + (b + c)$
- You can control this by only recursing on one side of the ENBF definition

Left-associative:
```ebnf
expr = prim | expr ('+' | '-' | '*' | '/') prim
```

Right-associative:
```ebnf
expr = prim | prim ('+' | '-' | '*' | '/') expr
```

## Operator precedence
If given an expression with multiple operators, operator precedence decides which operator to apply first. In real mathematics, we use the BIDMAS system for operator precedence, but a programming language may choose a different system

The following grammar has equal precedence for all operators:
```EBNF
expr = prim
	| expr ‘+’ prim
	| expr ‘-’ prim
	| expr ‘*’ prim
	
prim = num
	| id
	| ‘(’ expr ‘)’
```

In this grammar, $*$ has a higher precedence than $+$ and $-$. This is because multiplication operations are evaluated first (I.E at the bottom of the grammar tree of an `expr`):
```EBNF
expr = term
	| expr ‘+’ term
	| expr ‘-’ term
	
term = prim
	| term ‘*’ prim
	
prim = num
	| id
	| ‘(’ expr ‘)’
```


## Ambiguity
- A phrase is ambiguous if it has more than one syntax tree
- This means that it could be interpreted in two ways

> [!EXAMPLE]
> ```EBNF
> com = ‘put’ expr
> 	| ‘if’ expr ‘then’ com
> 	| ‘if’ expr ‘then’ com ‘else’ com
> ```
> 
>`if b then if c then put 1 else put 2` would be ambiguous because it could be interpreted as:
>1. ![[Pasted image 20240114051839.png]]
>2. ![[Pasted image 20240114051849.png]]

## Unary operators
An example for unary `-` (E.G $-4$)
```ebnf
expr = prim | unary_expr ('+' | '-' | '*' | '/') prim
unary_expr = '-' num | prim
```