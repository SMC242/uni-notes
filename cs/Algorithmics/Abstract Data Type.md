An abstract data type is an interface that a group of data structures implement. It specifies the basic members and operations of a data structure

An implementation of an ADT decides how to go about those operations. It can choose the underlying data structures and algorithms.

For example, a `Map` could be implemented with hashes or a binary tree, but both versions will implement `get(x)` and `remove(x)`.