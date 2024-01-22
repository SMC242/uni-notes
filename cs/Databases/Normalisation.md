# Overview
Normalisation is an algorithm for determining which attributes can be the [[Keys#Primary|primary]] and [[Keys#Foreign|foreign]] keys. Built on top of [[Functional Dependency Theory]]

# Algorithm
Progressively decomposing bad relations into smaller, better relations

1. Assert the functional dependencies
2. Put all the asserted FDs into a pool
3. Create a universal relation with all attributes
4. Recursively: apply the [[Functional Dependency Theory#Decomposition|Decomposition Rule]] to split the big relation into smaller ones.
	- Ensure that no information would be lost when joining
	- Ensure that no [[Database Design Guidelines#Fictitious tuples|fictitious tuples]] are generated when joining


# Normal forms
The degree of decomposition is known as the normal form (NF)

- [[#First Normal Form]]
- [[#Second Normal Form]]
- [[#Third Normal Form]]
	- [[#Generalised Third Normal Form]]
- [[#Boyce-Codd Normal Form]]
- [[#Fourth Normal Form]]
- [[#Fifth Normal Form]]
- [[#Sixth Normal Form]]

## First Normal Form
AKA 1NF

> The domain $D_{i}$ of each attribute $A_{i}$ in relation $R$ refers only to atomic values

Not allowed:
- Nested attributes
- Multi-valued attributes

Will have:
- Redundant and/or repeated values

## Second Normal Form
AKA 2NF

> Every non-[[Keys#Prime attributes|prime attribute]] $A$ in $R$ is [[Functional Dependency Theory#Full dependency|fully functionally dependent]] on the [[Keys#Primary|primary key]] of $R$

### Process
- Goal: remove all prime attributes from the primary key that cause partial dependencies (I.E not full dependencies)
- Requirement: the relation is already in 1NF

1. Find all the partial FDs
2. For each partial FD: create a new relation where all non-prime attributes are fully functionally dependent on a new primary key
	- The new key will be the prime attribute that caused the partial FD

## Third Normal Form
AKA 3NF

> There are no non-prime attribtues that are [[Functional Dependency Theory#Transitive|transitively dependent]] on the primary key

> [!NOTE] Transitive dependency
> If $X \rightarrow Z \land Z \rightarrow Y$, $X \rightarrow Y$ is a transitive dependency 
>
>Example: `COURSE(CourseID (PK), Lecturer, School)`
>- $CourseID \rightarrow Lecturer$
>- $Lecturer \rightarrow School$
>- Therefore $CourseID \rightarrow School$
>
>```mermaid
>graph LR
>	CourseID --> Lecturer
>	Lecturer --> School
>	CourseID --> School
> ```

### Process
Split the relation into 2 smaller ones

The non-prime transitive attribute becomes the:
- Primary key for the new relation
- Foreign key for the old relation
### Generalised Third Normal Form
AKA G3NF

## BCNF
AKA Boyce-Codd Normal Form

> Whenever there is an $FD : X \rightarrow A$, $X$ is a primary key

### BCNF Decomposition Theorem
Given a relation $R$ not in BCNF and an FD $X \rightarrow A$ that causes the BCNF violation:
1. Split $R$ into two relations:
	- $R_{1} = R \setminus \{A\}$: remove $A$ from the main relation
	- $R_{2} = \{X\} \cup \{A\}$: move $A$ to another relation and reference $X$
2. If $R_{1}$ or $R_{2}$ is not in BCNF, repeat

> [!EXAMPLE]
> `TEACH(Student (FK), Instructor (FK), Course (FK))`
>
>FDs:
> 1. $\{Student, Course\} \rightarrow Instructor$
> 2. $Instructor \rightarrow Course$  <-- violation
>
>Split into two relations:
>1. $R_{1} = \{Student, Instructor\}$
>2. $R_{2} = \{Instructor, Course\}$

