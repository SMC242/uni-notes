# Overview
Syntactic analysis is the process of checking that an input program conforms to the [[Syntax]] of a language. It also breaks the program down into [[Syntax#Grammar|phases]]

There are two components:
- The [[#lexer]] or "tokeniser"
	- Breaks the program into [[#tokens]]
- The [[#parser]]
	- Finds the phrase structure of the program
# Tokens
- Symbols that affect the phrase structure
- Tokens are made up of a tag and some text
	- Addition operator: `PLUS "+"`
	- Numeric literal: `NUM "1"`
	- Variable: `ID "x"`

> [!EXAMPLE] Examples of tokens
> - Literals
> - Identifiers
> - Operators
> - Keywords
> - Punctuation

## Separators
Text that doesn't affect the phrase structure such as:
- Spaces
- Comments
- Line breaks (EoLs)

Separators are not tokens

> [!WARNING] Line breaks
> Some programming languages classify EoLs as tokens

# Lexer
AKA tokeniser

- Streams tokens to the parser
- Applied one character at a time until the end of file (EoF token)
- Will have rules for what to do when certain tokens are found such as:
	- Spaces: discard
	- Start of comment: discard until end of comment
	- Digit: scan rest of digits
	- Letter: scan rest of digits (could output an identifier or a keyword)

# Parser
- Converts a token stream to an [[Abstract Syntax Tree|AST]]
- There are two strategies:
	- [[#Top-down parsing|Top-down]]
	- [[#Bottom-up parsing|Bottom-up]]

## Top-down parsing
AKA recursive-descent or backtracking

- Leverages [[recursion]] to process the stream
- Operates left-to-right

Recursive-descent parsers have:
- Parsing methods $N()$ for each [[Syntax#Grammar|non-terminal symbol]]
	- Checks if the next few tokens form a phrase of class $N$. If yes, consume and return [[Abstract Syntax Tree|AST]], errors if no
	- $N(t)$ matches a [[Syntax#Grammar|terminal symbol]]
- A $match(t)$ function
	- Checks if the next token has the tag $t$. Consumes if yes, errors if no