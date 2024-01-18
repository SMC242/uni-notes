# Overview
Data integrity refers to the validity of data (I.E that it isn't in an inconsistent state). There are three types of constraints used to ensure data integrity:
- [[#Key]]
- [[#Entity integrity]]
- [[#Referential integrity]]

# Constraints
## Key
Ensuring that records are uniquely identified by their keys

- A [[Keys#Primary key|primary key]] should uniquely identify its record

## Entity integrity
Preventing keys from being null

- Primary keys can't be `NULL`
- If the primary key is [[Keys#Composite|composite]], none of its attributes can be `NULL`

## Referential integrity
Ensuring that the interpretation of relationships remains valid

- If a record references another, the referenced key must exist
	- E.G if $R_{1}$ references $R_{2}$ and $R_{2}$'s key changes, $R_{1}$ must be updated to reflect that
- Otherwise, the foreign key field must be set to `NULL`
	- E.G if $R_{2}$ is deleted, $R_{1}$'s reference to $R_{2}$ should be set to `NULL`

## Semantic integrity
Constraints from the application such as "a student may not take >120 credits per year"
