# Overview
There are 3 fundamental operations:
- Insert
- Delete
- Update

There are 2 principles that must hold during these operations:
1. [[Data Integrity|Integrity constraints]] should not be violated
2. Operations may propagate and trigger other operations in order to maintain [[Data Integrity|integrity constraints]]. This ensures that the database moves between consistent states

See first: [[Data Integrity]]

# Handling violations
If an integrity constraint is violated, you could:
- Cancel the operation (reject/abort)
- Continue and warn the user
- Trigger other operations to fix it (cascade, set `NULL`)
- Execute a predefined error-correction procedure
- Don't report it

# Vectors for violations

## Insert
- Domain: one of the attribute values is not valid
	- E.G the attribute must be a number but a string was provided
- [[Data Integrity#Key|Key]]: key inserted that already exists
	- Solution: check whether the [[Keys|key]] exists before inserting
- [[Data Integrity#Referential integrity|Referential integrity]]: a [[Keys#Foreign|foreign key]] references a [[Keys#Primary|primary key]] that doesn't exist
- [[Data Integrity#Entity integrity|Entity integrity]]: the [[Keys#Primary|primary key]] is `NULL`

## Delete
- [[Data Integrity#Referential integrity|Referential integrity]]: deleting a [[Keys#Primary|primary key]] that is referenced by a [[Keys#Foreign|foreign key]]

## Update
- [[Data Integrity#Key|Key]]: duplicate primary key
- [[Data Integrity#Referential integrity|Referential integrity]]: referencing a key that doesn't exist