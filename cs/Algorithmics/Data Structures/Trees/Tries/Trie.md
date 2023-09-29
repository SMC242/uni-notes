---
tags:
  - ADT/Trie
---

# Definition
A Trie is a tree-like data structure used to efficiently store and retrieve a dynamic set of strings, typically with keys consisting of characters or strings. It is designed for fast retrieval and searching of words or strings.

Trie works by organizing data in a tree structure, where each node represents a character or a part of a string. Common prefixes are shared among branches, making it highly efficient for tasks like dictionary-based spell checking, autocomplete, and storing word dictionaries.

# Motivation
## Use case
Trie data structures are particularly useful in scenarios where you need to work with strings, such as:
- Dictionary implementations for spell checking.
- Implementing autocomplete features in search engines or text editors.
- Storing and retrieving a large set of words efficiently.

# Comparison
- Comparison to relevant structures
- Advantages and disadvantages \[in comparison to relevant structures\]

<ul class="breakdown">
	<li class="pro">Trie provides fast and efficient string retrieval and searching.</li>
	<li class="pro">It handles tasks like spell checking and autocomplete exceptionally well.</li>
	<li class="pro">Efficient storage of a large set of strings with common prefixes.</li>
	<li class="con">Trie can be memory-intensive, especially for large datasets.</li>
	<li class="con">It may not be the best choice for numerical or non-string data.</li>
</ul>

# Members
A Trie consists of nodes, where each node has the following attributes:
- Character: Represents a character in a string.
- Children: Stores references to child nodes, typically in an associative structure like a dictionary or an array.
- IsEndOfWord: A boolean flag that indicates whether the current node marks the end of a valid word.

# Operations
The operations defined for a Trie data structure include:

## Insertion
- Description: Adds a word to the Trie by creating nodes for each character and marking the last node as the end of the word.

## Search
- Description: Checks if a word exists in the Trie by traversing the nodes and verifying the presence of the IsEndOfWord flag.

## Prefix Search
- Description: Searches for words with a specific prefix by traversing the Trie until the desired prefix is reached and then exploring all possible branches.

## Deletion
- Description: Removes a word from the Trie by deleting nodes corresponding to each character and removing the IsEndOfWord flag of the last node.

# Optional operations
These operations do not need to be implemented, but they can be useful for specific use cases or optimizations. Some optional operations include:
- Count words: Count the number of words stored in the Trie.
- Longest common prefix: Find the longest common prefix among a set of words in the Trie.](<---
tags:
- ADT/Trie

---
# Definition
A blurb about the data structure and a high-level description of how it works.

# Motivation
## Use case
When would you use this structure?

# Comparison
- Comparison to relevant structures
- Advantages and disadvantages \[in comparison to relevant structures\]

%3Cul class="breakdown"%3E
	<li class="pro">Pro</li>
	<li class="con">Con</li>
</ul>

# Members
The attributes of an instance of this data structure and any sub-objects used

# Operations
The operations that are defined for this structure and what they do

## Example operation
- Description


# Optional operations
These operations do not need to be implemented

# Implementations
All of implementations of this [[Abstract Data Type|ADT]]:

> [!WARNING]
> Edit the tag in this query
```dataview
LIST
FROM
	#ADT/NAME-OF-ADT 
WHERE
	file.name != this.file.name
`````>)