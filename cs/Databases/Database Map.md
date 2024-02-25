# Overview
Databases are systems designed to efficiently store, read, write, and update data. They revolve around providing a few services:

![[Fundamental operations#Overview]]

# Concepts
- [[Fundamental operations]]: the core operations of a database
- [[Keys]]: how relations are linked together into relationships

# Database design
Designing good database schemas

- [[Data Integrity]]: ensuring that data doesn't rot
- [[Database Design Guidelines]]
- [[Functional Dependency Theory]]: evaluating the quality of a relational model
- [[Normalisation]]: a series of algorithms for making good models

# SQL
Structure Query Language is a declarative language for querying databases

- [[Joins]]: how to fetch data from multiple tables

# Storage
How databases are stored on disk

- [[Database Files]]: how databases are stored efficiently
	- [[Storage Blocks]]: how files are represented on disk
- [[Indexing]]: making references to data to enable efficient queries
	- [[Multilevel Index]]: representing indexes as trees
		- [[B+ Tree]]: a type of tree optimised for storing databases