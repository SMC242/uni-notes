# Overview
Where [[Database Files|physical methods]] for efficiently storing data only support one field, indexing supports an unlimited number of sorting fields. This allows efficient queries over multiple fields

> [!NOTE] Silver bullet?
> Physical methods are not useless: they provide a *primary* access path whereas indexes are a *secondary* access path
> 
> Indexes are faster, but create a maintenance burden

# Properties
- Indexes are stored in separate files from the data
	- Smaller than the real data because they're just pointers and an index value
	- This provides a greater *density* (I.E greater [[Storage Blocks#Blocking factor|bfr]]):  there are more entries per block
- Each index applies to one field
- Index entries are:
	- Unique
	- Sorted
		- This means you can use a [[Binary Search]]
- Index entries are stored as a tuple of `index value, block pointer)`

# Access process
1. Search the index for the value
2. Get the [[Storage Blocks#Blocks|block pointer]] from that index entry
3. Follow it to the [[Database Files|block in the database file]]
4. Load the block into memory

# Index density
- Dense index: one index entry per record
- Sparse index: only some records have an index entry

# Types
## Primary index
- When the index field is the ordering + key field of the corresponding [[Database Files#Ordered files|sequential file]]

## Clustering index
- When the index field is the ordering field *but not* the key field of the corresponding [[Database Files#Ordered files|sequential file]]

## Secondary index
- When the index field is a non-ordering field in a [[Database Files#Ordered files|sequential file]] or [[Database Files#Heap files|unordered file]]
	- Can be a key, doesn't matter